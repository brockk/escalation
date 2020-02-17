
#' Get an object to fit the BOIN model using the BOIN package.
#'
#' @param num_doses Number of doses under investigation.
#' @param target We seek a dose with this probability of toxicity.
#' @param use_stopping_rule TRUE to use the toxicity stopping rule described in
#' Yan et al. (2019). FALSE to suppress the authors' stopping rule, with the
#' assumption being that you will test the necessity to stop early in some other
#' way.
#' @param ... Extra args are passed to \code{\link[BOIN]{select.mtd}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' BOIN model to outcomes.
#'
#' @export
#'
#' @examples
#' target <- 0.25
#' model1 <- get_boin(num_doses = 5, target = target)
#'
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' @references
#' Yan, F., Pan, H., Zhang, L., Liu, S., & Yuan, Y. (2019).
#' BOIN: An R Package for Designing Single-Agent and Drug-Combination
#' Dose-Finding Trials Using Bayesian Optimal Interval Designs.
#' Journal of Statistical Software, 27(November 2017), 0–35.
#' https://doi.org/10.18637/jss.v000.i00
#'
#' Liu, S., & Yuan, Y. (2015).
#' Bayesian optimal designs for Phase I clinical trials.
#' J. R. Stat. Soc. C, 64, 507–523.
#' https://doi.org/10.1111/rssc.12089
get_boin <- function(num_doses, target, use_stopping_rule = TRUE, ...) {

  x <- list(
    num_doses = num_doses,
    target = target,
    use_stopping_rule = use_stopping_rule,
    extra_args = list(...)
  )

  class(x) <- c('boin_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

#' @importFrom BOIN select.mtd get.boundary
#' @importFrom utils tail
#' @importFrom magrittr %>%
boin_selector <- function(outcomes, num_doses, target, use_stopping_rule, ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  df_c <- model_frame_to_counts(df, num_doses = num_doses)
  x <- select.mtd(target = target, npts = df_c$n, ntox = df_c$tox, ...)

  # Checks
  if(nrow(df) > 0) {
    if(max(df$dose) > num_doses) {
      stop('boin_selector - maximum dose given exceeds number of doses.')
    }
  }

  if(nrow(df) == 0) {
    recommended_dose <- 1
    continue <- TRUE
  } else {
    last_dose <- df$dose %>% tail(1)
    n_d <- df_c$n[last_dose]
    tox_d <- df_c$tox[last_dose]
    bound <- get.boundary(target = target, ncohort = 1,
                          cohortsize = n_d + 1)
    this_bound <- bound$full_boundary_tab[, n_d]
    if(tox_d <= this_bound['Escalate if # of DLT <=']) {
      # Escalate, if possible
      recommended_dose <- pmin(num_doses, last_dose + 1)
      continue <- TRUE
    } else if(tox_d >= this_bound['Deescalate if # of DLT >=']) {
      # De-escalate and possibly eliminate
      if(use_stopping_rule &
         !is.na(this_bound['Eliminate if # of DLT >=']) &
         tox_d >= this_bound['Eliminate if # of DLT >=']){

        # TODO is elimination path-dependent?? If so this approach could fail.

        if(last_dose == 1) {
          # Stop and recommend no dose.
          recommended_dose <- NA
          continue <- FALSE
        } else {
          # Eliminate this dose and all higher doses.
          # Select highest non-elimnated dose
          recommended_dose <- last_dose - 1
          continue <- TRUE
        }
      } else {
        # De-escalate, if possible
        recommended_dose <- pmax(1, last_dose - 1)
        continue <- TRUE
      }
    } else {
      # As you were
      recommended_dose <- last_dose
      continue <- TRUE
    }
  }

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    num_doses = as.integer(num_doses),
    target = target,
    boin_fit = x,
    df = df,
    df_c = df_c,
    recommended_dose = recommended_dose,
    continue = continue
  )

  class(l) = c('boin_selector', 'tox_selector', 'selector')
  l
}

# Factory interface

#' @export
fit.boin_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    target = selector_factory$target,
    use_stopping_rule = selector_factory$use_stopping_rule
  )
  args <- append(args, selector_factory$extra_args)
  do.call(boin_selector, args = args)
}

# Selector interface

#' @export
num_patients.boin_selector <- function(selector, ...) {
  return(length(selector$df$dose))
}

#' @export
cohort.boin_selector <- function(selector, ...) {
  return(selector$df$cohort)
}

#' @export
doses_given.boin_selector <- function(selector, ...) {
  return(selector$df$dose)
}

#' @export
tox.boin_selector <- function(selector, ...) {
  return(selector$df$tox)
}

#' @export
num_doses.boin_selector <- function(selector, ...) {
  return(selector$num_doses)
}

#' @export
recommended_dose.boin_selector <- function(selector, ...) {
  return(as.integer(selector$recommended_dose))
}

#' @export
continue.boin_selector <- function(selector, ...) {
  return(selector$continue)
}

#' @export
n_at_dose.boin_selector <- function(selector, ...) {
  return(selector$df_c$n)
}

#' @export
tox_at_dose.boin_selector <- function(selector, ...) {
  return(selector$df_c$tox)
}

#' @export
mean_prob_tox.boin_selector <- function(selector, ...) {
  # The authors store prob(DLT) as an ordered variable with the probs as levels:
  # They also use '----' to show a dose has never been given.
  mean_s <- as.character(selector$boin_fit$p_est$phat)
  mean_s[mean_s == '----'] <- NA
  return(as.numeric(mean_s))
}

#' @export
median_prob_tox.boin_selector <- function(selector, ...) {
  prob_tox_quantile(selector, p = 0.5, ...)
}

#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr mutate group_by slice ungroup select arrange
#' @importFrom tidyr unnest
#' @export
prob_tox_quantile.boin_selector <- function(
  selector, p,
  quantile_candidates = seq(0, 1, length.out = 101),
  ...) {

  dose <- prob <- . <- distance <- NULL

  # x <- tibble(
  #   q = quantile_candidates
  # ) %>% mutate(
  #   dose = map(q, .f = ~ dose_indices(selector)),
  #   prob = map(q, .f = ~ 1 - prob_tox_exceeds(selector, threshold = .x))
  # ) %>% unnest(cols = c(dose, prob)) %>%
  #   group_by(dose) %>%
  #   slice(which.min(abs(prob - p))) %>%
  #   ungroup() %>%
  #   select(q) %>% .[[1]]
  # names(x) <- dose_indices(selector)

  x <- tibble(
    q = quantile_candidates
  ) %>% mutate(
    dose = map(q, .f = ~ dose_indices(selector)),
    prob = map(q, .f = ~ 1 - prob_tox_exceeds(selector, threshold = .x))
  ) %>% unnest(cols = c(dose, prob)) %>%
    mutate(distance = abs(prob - p)) %>%
    arrange(dose, distance) %>%
    group_by(dose) %>%
    slice(1) %>%
    ungroup() %>%
    select(q) %>% .[[1]]
  x[n_at_dose(selector) == 0] <- NA
  names(x) <- dose_indices(selector)
  x
}

#' @importFrom stats pbeta
#' @importFrom purrr map_dbl
#' @export
prob_tox_exceeds.boin_selector <- function(selector, threshold, ...) {
  # The authors use beta-binomial conjugate approach. They use a
  # Beta(0.05, 0.05) prior. I could not find an explanation why.
  # They also use isotonic regression to ensure Either way:
  prob_od <- pbeta(q = threshold,
                   shape1 = 0.05 + tox_at_dose(selector),
                   shape2 = 0.05 + n_at_dose(selector) - tox_at_dose(selector),
                   lower.tail = FALSE)
  names(prob_od) <- 1:num_doses(selector)
  given <- n_at_dose(selector) > 0
  prob_od2 <- boin_pava(prob_od[given])
  prob_od3 <- map_dbl(
    1:num_doses(selector),
    ~ ifelse(.x %in% names(prob_od2), prob_od2[as.character(.x)], NA)
  )
  return(prob_od3)
}

#' @export
supports_sampling.boin_selector <- function(selector, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.boin_selector <- function(selector, tall = FALSE, ...) {
  stop('boin_selector does not support sampling.')
}
