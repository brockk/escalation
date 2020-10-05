
#' Get an object to fit the TPI dose-finding model.
#'
#' @param num_doses Number of doses under investigation.
#' @param target We seek a dose with this probability of toxicity.
#' @param k1 The K1 parameter in TPI determines the lower bound of the
#' equivalence interval. See Details.
#' @param k2 The K2 parameter in TPI determines the upper bound of the
#' equivalence interval. See Details.
#' @param exclusion_certainty Numeric, threshold posterior certainty required to
#' exclude a dose for being excessively toxic. The authors discuss values in the
#' range 0.7 - 0.95. Set to a value > 1 to suppress the dose exclusion
#' mechanism.
#' @param alpha First shape parameter of the beta prior distribution on the
#' probability of toxicity.
#' @param beta Second shape parameter of the beta prior distribution on the
#' probability of toxicity.
#' @param ... Extra args are passed onwards.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' TPI model to outcomes.
#'
#' @export
#'
#' @examples
#' target <- 0.25
#' model1 <- get_tpi(num_doses = 5, target = target, k1 = 1, k2 = 1.5,
#'   exclusion_certainty = 0.95)
#'
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' @references
#' Ji, Y., Li, Y., & Bekele, B. N. (2007).
#' Dose-finding in phase I clinical trials based on toxicity probability
#' intervals.
#' Clinical Trials, 4(3), 235–244. https://doi.org/10.1177/1740774507079442
#'
#' Ji, Y., & Yang, S. (2017).
#' On the Interval-Based Dose-Finding Designs, 1–26.
#' Retrieved from http://arxiv.org/abs/1706.03277
get_tpi <- function(num_doses, target,
                    k1, k2,
                    exclusion_certainty,
                    alpha = 0.005, beta = 0.005,
                    ...) {

  x <- list(
    num_doses = num_doses,
    target = target,
    k1 = k1,
    k2 = k2,
    exclusion_certainty = exclusion_certainty,
    alpha = alpha,
    beta = beta,
    extra_args = list(...)
  )

  class(x) <- c('tpi_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

tpi_selector <- function(outcomes, num_doses, target,
                         k1, k2,
                         exclusion_certainty,
                         alpha, beta,
                         ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  df_c <- model_frame_to_counts(df, num_doses = num_doses)

  # Checks
  if(nrow(df) > 0) {
    if(max(df$dose) > num_doses) {
      stop('tpi_selector - maximum dose given exceeds number of doses.')
    }
  }

  if(nrow(df) == 0) {
    recommended_dose <- 1
    continue <- TRUE
  } else {
    last_dose <- df$dose %>% tail(1)
    n_d <- df_c$n[last_dose]
    tox_d <- df_c$tox[last_dose]
    post_alpha <- alpha + tox_d
    post_beta <- beta + n_d - tox_d
    post_var <- (post_alpha * post_beta) /
      ((post_alpha + post_beta)^2 * (post_alpha + post_beta + 1))
    post_sd <- sqrt(post_var)
    prob_unsafe <- pbeta(target, post_alpha, post_beta, lower.tail = FALSE)
    ei_lower <- max(target - k2 * post_sd, 0)
    ei_upper <- min(target + k1 * post_sd, 1)
    # prob_ui, prob_ei & prob_oi are the posterior probabilities that the true
    # tox rate at given dose is in the underdose, equivalent-dose, and overdose
    # regions. They form a partition: prob_ui + prob_ei + prob_oi = 1
    prob_ui <- pbeta(ei_lower, post_alpha, post_beta, lower.tail = TRUE)
    prob_ei <- pbeta(ei_upper, post_alpha, post_beta, lower.tail = TRUE) -
      prob_ui
    prob_oi <- 1 - prob_ui - prob_ei

    if(last_dose < num_doses) {
      # Escalation is possible.
      # Scale prob_ei by the identity function measuring whether the next dose
      # is excessively toxic. This prevents escalation to excluded doses.
      d_plus_1 <- last_dose + 1
      n_d2 <- df_c$n[d_plus_1]
      if(n_d2 >= 2) {
        tox_d2 <- df_c$tox[d_plus_1]
        post_alpha2 <- alpha + tox_d2
        post_beta2 <- beta + n_d2 - tox_d2
        prob_unsafe2 <- pbeta(target, post_alpha2, post_beta2,
                              lower.tail = FALSE)
        if(prob_unsafe2 >= exclusion_certainty) {
          # We cannot escalate to an unsafe dose, so move the escalate weight to
          # the stay weight and set the escalate weight to zero.
          prob_ei <- prob_ei + prob_ui
          prob_ui <- 0
        }
        # prob_ui <- prob_ui * as.integer(prob_unsafe2 < exclusion_certainty)
      }
    }

    if(prob_ui > pmax(prob_ei, prob_oi)) {
      # Escalate if possible
      recommended_dose <- min(num_doses, last_dose + 1)
      continue <- TRUE
    } else if(prob_ei > pmax(prob_ui, prob_oi)) {
      # Stick at last dose
      recommended_dose <- last_dose
      continue <- TRUE
    } else if(prob_oi > pmax(prob_ui, prob_ei)) {
      # De-escalate if possible.
      if(last_dose > 1) {
        recommended_dose <- last_dose - 1
        continue <- TRUE
      } else {
        # We are at lowest dose. Stop trial if lowest dose is excessively toxic.
        if(prob_unsafe > exclusion_certainty) {
          recommended_dose <- NA
          continue <- FALSE
        } else {
          recommended_dose <- 1
          continue <- TRUE
        }
      }
    } else {
      stop('Hypothetically infeasible situation in tpi_selector.')
    }
  }

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    num_doses = as.integer(num_doses),
    target = target,
    df = df,
    df_c = df_c,
    alpha = alpha,
    beta = beta,
    exclusion_certainty = exclusion_certainty,
    k1 = k1,
    k2 = k2,
    recommended_dose = recommended_dose,
    continue = continue
  )

  class(l) = c('tpi_selector', 'tox_selector', 'selector')
  l
}


# Factory interface

#' @export
fit.tpi_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    target = selector_factory$target,
    k1 = selector_factory$k1,
    k2 = selector_factory$k2,
    exclusion_certainty = selector_factory$exclusion_certainty,
    alpha = selector_factory$alpha,
    beta = selector_factory$beta
  )
  args <- append(args, selector_factory$extra_args)
  do.call(tpi_selector, args = args)
}

# Selector interface

#' @export
tox_target.tpi_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.tpi_selector <- function(x, ...) {
  return(length(x$df$dose))
}

#' @export
cohort.tpi_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.tpi_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.tpi_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
num_doses.tpi_selector <- function(x, ...) {
  return(x$num_doses)
}

#' @export
recommended_dose.tpi_selector <- function(x, ...) {
  return(as.integer(x$recommended_dose))
}

#' @export
continue.tpi_selector <- function(x, ...) {
  return(x$continue)
}

#' @export
tox_at_dose.tpi_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
mean_prob_tox.tpi_selector <- function(x, ...) {

  post_mean = (x$alpha + tox_at_dose(x)) / (x$alpha + x$beta + n_at_dose(x))
  post_var = (x$alpha + tox_at_dose(x)) *
    (x$beta + n_at_dose(x) - tox_at_dose(x)) /
    ((x$alpha + x$beta + n_at_dose(x))^2 * (x$alpha + x$beta + n_at_dose(x) + 1))
  post_mean = pava(post_mean, wt = 1 / post_var)
  return(post_mean)
}

#' @export
median_prob_tox.tpi_selector <- function(x, ...) {
  prob_tox_quantile(x, p = 0.5, ...)
}

#' @export
dose_admissible.tpi_selector <- function(x, ...) {
  n_d <- n_at_dose(x)
  t_d <- tox_at_dose(x)
  prob_unsafe <- prob_tox_exceeds(x, threshold = x$target)
  reject <- logical(length = num_doses(x))
  for(i in seq_along(reject)) {
    if(n_d[i] >= 2) {
      reject[i] <- prob_unsafe[i] >= x$exclusion_certainty
    } else {
      reject[i] <- FALSE # Implicitly
    }
  }
  # However, monotonic tox suggests doses higher than an inadmissible dose
  # are also inadmissible:
  cum_reject <- cumsum(reject) >= 1
  return(!cum_reject)
}

#' @export
prob_tox_quantile.tpi_selector <- function(
  x, p, quantile_candidates = seq(0, 1, length.out = 101), ...) {
  reverse_engineer_prob_tox_quantile(x, p, quantile_candidates, ...)
}

#' @export
prob_tox_exceeds.tpi_selector <- function(x, threshold, ...) {
  pava_bb_prob_tox_exceeds(x, threshold, alpha = x$alpha, beta = x$beta)
}

#' @export
supports_sampling.tpi_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.tpi_selector <- function(x, tall = FALSE, ...) {
  stop('tpi_selector does not support sampling.')
}
