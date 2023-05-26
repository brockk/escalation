
#' Get an object to fit the mTPI-2 dose-finding model.
#'
#' The modified toxicity probability interval 2 (mTPI-2) is a dose-escalation
#' design by Guo et al. As the name suggests, it is an adaptation of the mTPI
#' design.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param num_doses Number of doses under investigation.
#' @param target We seek a dose with this probability of toxicity.
#' @param epsilon1 This parameter determines the lower bound of the
#' equivalence interval. See Details.
#' @param epsilon2 This parameter determines the upper bound of the
#' equivalence interval. See Details.
#' @param exclusion_certainty Numeric, threshold posterior certainty required to
#' exclude a dose for being excessively toxic. The authors discuss values in the
#' range 0.7 - 0.95. Set to a value > 1 to suppress the dose exclusion
#' mechanism. The authors use the Greek letter xi for this parameter.
#' @param alpha First shape parameter of the beta prior distribution on the
#' probability of toxicity.
#' @param beta Second shape parameter of the beta prior distribution on the
#' probability of toxicity.
#' @param ... Extra args are passed onwards.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' mTPI-2 model to outcomes.
#'
#' @section Details:
#' The design seeks a dose with probability of toxicity \eqn{p_{i}}
#' close to a target probability \eqn{p_{T}} by iteratively calculating the
#' interval \deqn{p_{T} - \epsilon_{1} < p_{i} < p_{T} + \epsilon_{2}}
#' In this model, \eqn{\epsilon_{1}} and \eqn{\epsilon_{2}} are specified
#' constants. \eqn{p_{i}} is estimated by a Bayesian beta-binomial conjugate
#' model \deqn{p_{i} | data \sim Beta(\alpha + x_{1}, \beta + n_{i} - x_{i}),}
#' where \eqn{x_{i}} is the number of toxicities observed and \eqn{n_{i}} is the
#' number of patients treated at dose \eqn{i}, and \eqn{\alpha} and \eqn{\beta}
#' are hyperparameters for the beta prior on \eqn{p_{i}}.
#' A dose is excluded as inadmissible if
#' \deqn{P(p_{i} > p_{T} | data) > \xi}
#' The trial commences at a starting dose, possibly dose 1. If dose \eqn{i}
#' has just been evaluated in patient(s), dose selection decisions proceed by
#' calculating the unit probability mass of the true toxicity rate at dose
#' \eqn{i} using the partition of the probability space into subintervals with
#' equal length given by\eqn{(\epsilon_{1} + \epsilon_{2})}. \eqn{EI} is the
#' equivalence interval \eqn{p_{T} - epsilon_{1}, p_{T} - epsilon_{2}}, with
#' \eqn{LI} the set of all intervals below, and \eqn{HI} the set of all
#' intervals above.
#' The unit probability mass (UPM) of an interval is the posterior probability
#' that the true toxicity rate belongs to the interval divided by the width of
#' the interval. The interval with maximal UPM determines the recommendation for
#' the next patient(s), with the intervals corresponding to decisions to
#' escalate, stay, and de-escalate dose, respectively. Further to this are rules
#' that prevent escalation to an inadmissible dose.
#' In the original mTPI paper, the authors demonstrate acceptable operating
#' performance using \eqn{\alpha = \beta = 1}, \eqn{K_{1} = 1},
#' \eqn{K_{2} = 1.5} and  \eqn{\xi = 0.95}.
#' The authors of the mTPI-2 approach show desirable performance as compared
#' to the original mTPI method, under particular parameter choices.
#' See the publications for full details.
#'
#' @export
#'
#' @examples
#' target <- 0.25
#' model1 <- get_mtpi2(num_doses = 5, target = target, epsilon1 = 0.05,
#'   epsilon2 = 0.05, exclusion_certainty = 0.95)
#'
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' @references
#' Ji, Y., Liu, P., Li, Y., & Bekele, B. N. (2010).
#'  A modified toxicity probability interval method for dose-finding trials.
#'  Clinical Trials, 7(6), 653–663. https://doi.org/10.1177/1740774510382799
#'
#' Ji, Y., & Yang, S. (2017).
#' On the Interval-Based Dose-Finding Designs, 1–26.
#' Retrieved from https://arxiv.org/abs/1706.03277
#'
#' Guo, W., Wang, SJ., Yang, S., Lynn, H., Ji, Y. (2017).
#' A Bayesian Interval Dose-Finding Design Addressing Ockham's Razor: mTPI-2.
#' https://doi.org/10.1016/j.cct.2017.04.006
#'
get_mtpi2 <- function(parent_selector_factory = NULL, num_doses, target,
                     epsilon1, epsilon2,
                     exclusion_certainty,
                     alpha = 1, beta = 1,
                     ...) {

  x <- list(
    parent_selector_factory = parent_selector_factory,
    num_doses = num_doses,
    target = target,
    epsilon1 = epsilon1,
    epsilon2 = epsilon2,
    exclusion_certainty = exclusion_certainty,
    alpha = alpha,
    beta = beta,
    extra_args = list(...)
  )

  class(x) <- c('mtpi2_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

mtpi2_selector <- function(parent_selector = NULL, outcomes, num_doses, target,
                          epsilon1, epsilon2,
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
      stop('mtpi2_selector - maximum dose given exceeds number of doses.')
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
    # post_var <- (post_alpha * post_beta) /
    #   ((post_alpha + post_beta)^2 * (post_alpha + post_beta + 1))
    # post_sd <- sqrt(post_var)
    prob_unsafe <- pbeta(target, post_alpha, post_beta, lower.tail = FALSE)
    delta <- epsilon1 + epsilon2
    ei_lower_lim <- max(target - epsilon1, 0)
    lower <- upper <- NULL
    ei_lower <- data.frame(upper = matrix(data = seq(from = ei_lower_lim,
                                                     to = 0, by = -delta),
                       ncol = 1, byrow = T)) %>%
      mutate(lower = pmax(upper - delta, 0),
             delta = upper - lower) %>%
      filter(delta != 0)
    ei_upper_lim <- min(target + epsilon2, 1)
    ei_upper <- data.frame(lower = matrix(data = seq(from = ei_upper_lim,
                                                     to = 1, by = delta),
                                          ncol = 1, byrow = T)) %>%
      mutate(upper = pmin(lower + delta, 1),
             delta = upper - lower) %>%
      filter(delta != 0)
    # prob_ui, prob_ei & prob_oi are the posterior probabilities that the true
    # tox rate at given dose is in the (multiple) underdose, equivalent-dose,
    # and (multiple) overdose regions. They form a partition: prob_ui_i +
    # prob_ei + prob_oi_i = 1
    prob_ui <- apply(ei_lower, 1,
                     FUN = function(x){pbeta(x[1], post_alpha, post_beta,
                                             lower.tail = TRUE) -
        pbeta(x[2], post_alpha, post_beta, lower.tail = TRUE)})

    prob_ei <- pbeta(ei_upper_lim, post_alpha, post_beta, lower.tail = TRUE) -
      pbeta(ei_lower_lim, post_alpha, post_beta, lower.tail = TRUE)


    prob_oi <- apply(ei_upper, 1,
                     FUN = function(x){pbeta(x[2], post_alpha, post_beta,
                                             lower.tail = TRUE) -
                         pbeta(x[1], post_alpha, post_beta, lower.tail = TRUE)})


    # The UPM are the posterior intervals probabilities divided by the interval
    # width
    upm_ui <- prob_ui / (ei_lower$delta)
    upm_ei <- prob_ei / (ei_upper_lim - ei_lower_lim)
    upm_oi <- prob_oi / (ei_upper$delta)

    if(last_dose < num_doses) {
      # Escalation is possible.
      # Scale upm_ei by the identity function measuring whether the next dose
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
          # We cannot escalate to an unsafe dose, so set upm_ui to zero to
          # prevent escalation.
          upm_ui <- 0
        }
      }
    }

    if(max(upm_ei, upm_oi, upm_ui) %in% upm_ui) {
      # Escalate if possible
      recommended_dose <- min(num_doses, last_dose + 1)
      continue <- TRUE
    } else if(max(upm_ei, upm_oi, upm_ui) == upm_ei) {
      # Stick at last dose
      recommended_dose <- last_dose
      continue <- TRUE
    } else if(max(upm_ei, upm_oi, upm_ui) %in% upm_oi) {
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
      stop('Hypothetically infeasible situation in mtpi2_selector.')
    }
  }


  l <- list(
    parent = parent_selector,
    cohort = df$cohort,
    outcomes = outcomes,
    num_doses = as.integer(num_doses),
    target = target,
    df = df,
    df_c = df_c,
    alpha = alpha,
    beta = beta,
    exclusion_certainty = exclusion_certainty,
    epsilon1 = epsilon1,
    epsilon2 = epsilon2,
    recommended_dose = recommended_dose,
    continue = continue
  )

  class(l) = c('mtpi2_selector', 'tox_selector', 'selector')
  l
}


# Factory interface

#' @export
fit.mtpi2_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    target = selector_factory$target,
    epsilon1 = selector_factory$epsilon1,
    epsilon2 = selector_factory$epsilon2,
    exclusion_certainty = selector_factory$exclusion_certainty,
    alpha = selector_factory$alpha,
    beta = selector_factory$beta
  )
  args <- append(args, selector_factory$extra_args)
  do.call(mtpi2_selector, args = args)
}

# Selector interface

#' @export
tox_target.mtpi2_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.mtpi2_selector <- function(x, ...) {
  return(length(x$df$dose))
}

#' @export
cohort.mtpi2_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.mtpi2_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.mtpi2_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
num_doses.mtpi2_selector <- function(x, ...) {
  return(x$num_doses)
}

#' @export
recommended_dose.mtpi2_selector <- function(x, ...) {

  if(!is.null(x$parent)) {
    parent_dose <- recommended_dose(x$parent)
    parent_cont <- continue(x$parent)
    if(parent_cont & !is.na(parent_dose)) {
      return(parent_dose)
    }
  }

  # By default:
  return(as.integer(x$recommended_dose))
}

#' @export
continue.mtpi2_selector <- function(x, ...) {
  return(x$continue)
}

#' @export
tox_at_dose.mtpi2_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
mean_prob_tox.mtpi2_selector <- function(x, ...) {

  post_mean = (x$alpha + tox_at_dose(x)) / (x$alpha + x$beta + n_at_dose(x))
  post_var = (x$alpha + tox_at_dose(x)) *
    (x$beta + n_at_dose(x) - tox_at_dose(x)) /
    ((x$alpha + x$beta + n_at_dose(x))^2 * (x$alpha + x$beta + n_at_dose(x) + 1))
  post_mean = pava(post_mean, wt = 1 / post_var)
  return(post_mean)
}

#' @export
median_prob_tox.mtpi2_selector <- function(x, ...) {
  prob_tox_quantile(x, p = 0.5, ...)
}

#' @export
dose_admissible.mtpi2_selector <- function(x, ...) {
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
prob_tox_quantile.mtpi2_selector <- function(
  x, p, quantile_candidates = seq(0, 1, length.out = 101), ...) {
  reverse_engineer_prob_tox_quantile(x, p, quantile_candidates, ...)
}

#' @export
prob_tox_exceeds.mtpi2_selector <- function(x, threshold, ...) {
  pava_bb_prob_tox_exceeds(x, threshold, alpha = x$alpha, beta = x$beta)
}

#' @export
supports_sampling.mtpi2_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.mtpi2_selector <- function(x, tall = FALSE, ...) {
  stop('mtpi2_selector does not support sampling.')
}
