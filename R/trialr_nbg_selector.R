
#' Get an object to fit the NBG dose-finding model using the trialr package.
#'
#' @description
#' This function returns an object that can be used to fit a Neuenschwander,
#' Branson and Gsponer (NBG) model for dose-finding using methods provided by
#' the trialr package.
#'
#' @details
#' The model form implemented in trialr is:
#'
#' \eqn{F(x_{i}, \alpha, \beta) = 1 / (1 + \exp{-(\alpha + \exp{(\beta)} log(x_i / d_*))}) }
#'
#' with normal priors on alpha and beta.
#'
#' Dose selectors are designed to be daisy-chained together to achieve different
#' behaviours. This class is a **resumptive** selector, meaning it carries on
#' when the previous dose selector, where present, has elected not to continue.
#' For example, this allows instances of this class to be preceded by a selector
#' that follows a fixed path in an initial escalation plan, such as that
#' provided by \code{\link{follow_path}}. In this example, when the observed
#' trial outcomes deviate from that initial plan, the selector following the
#' fixed path elects not to continue and responsibility passes to this class.
#' See examples under \code{\link{get_dfcrm}}.
#'
#' A time-to-event variant, like TITE-CRM, is used when you specify
#' \code{tite = TRUE}. This weights the observations to allow dose-selections
#' based on partially observed outcomes.
#'
#' @param parent_selector_factory optional object of type
#' \code{\link{selector_factory}} that is in charge of dose selection before
#' this class gets involved. Leave as NULL to just use this model from the start.
#' @param real_doses Doses under investigation, a non-decreasing vector of
#' numbers.
#' @param d_star Numeric, reference dose for calculating the covariate
#' \code{log(dose / d_star)} when fitting the model. Sometimes (but not always)
#' taken to be the max dose in real_doses.
#' @param target We seek a dose with this probability of toxicity.
#' @param alpha_mean Prior mean of intercept variable for normal prior.
#' See Details. Also see documentation for trialr package for further details.
#' @param alpha_sd Prior standard deviation of intercept variable for normal prior.
#' See Details. Also see documentation for trialr package for further details.
#' @param beta_mean Prior mean of gradient variable for normal prior.
#' See Details. Also see documentation for trialr package for further details.
#' @param beta_sd Prior standard deviation of slope variable for normal prior.
#' See Details. Also see documentation for trialr package for further details.
#' @param ... Extra args are passed to \code{\link[trialr]{stan_nbg}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' NBG model to outcomes.
#'
#' @importFrom gtools inv.logit
#' @export
#'
#' @examples
#' real_doses <- c(5, 10, 25, 40, 60)
#' d_star <- 60
#' target <- 0.25
#'
#' model <- get_trialr_nbg(real_doses = real_doses, d_star = d_star,
#'                         target = target,
#'                         alpha_mean = 2, alpha_sd = 1,
#'                         beta_mean = 0.5, beta_sd = 1)
#' # Refer to the trialr documentation for more details on model & priors.
#' outcomes <- '1NNN 2NTN'
#' fit <- model %>% fit(outcomes)
#' fit %>% recommended_dose()
#' fit %>% mean_prob_tox()
#'
#' @references
#' Neuenschwander, B., Branson, M., & Gsponer, T. (2008).
#' Critical aspects of the Bayesian approach to phase I cancer trials.
#' Statistics in Medicine, 27, 2420–2439. https://doi.org/10.1002/sim.3230
#'
#' Brock, K. (2020). trialr: Clinical Trial Designs in 'rstan'.
#' R package version 0.1.5. https://github.com/brockk/trialr
#'
#' Brock, K. (2019). trialr: Bayesian Clinical Trial Designs in R and Stan.
#' arXiv preprint arXiv:1907.00161.
get_trialr_nbg <- function(parent_selector_factory = NULL, real_doses, d_star,
                           target,
                           alpha_mean, alpha_sd,
                           beta_mean, beta_sd,
                           tite = FALSE,
                           ...) {

  x <- list(
    parent_selector_factory = parent_selector_factory,
    real_doses = real_doses,
    d_star = d_star,
    target = target,
    alpha_mean = alpha_mean,
    alpha_sd = alpha_sd,
    beta_mean = beta_mean,
    beta_sd = beta_sd,
    tite = tite,
    extra_args = list(...)
  )

  class(x) <- c('trialr_nbg_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

#' Get an object to fit a TITE version of the NBG dose-finding model using trialr
#'
#' @inheritParams get_trialr_nbg
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' NBG model to outcomes.
#'
#' @export
#'
#' @examples
#' # TODO
get_trialr_nbg_tite <- function(parent_selector_factory = NULL,
                                real_doses,
                                d_star,
                                target,
                                alpha_mean, alpha_sd,
                                beta_mean, beta_sd,
                                ...) {
  return(
    get_trialr_nbg(
      parent_selector_factory = parent_selector_factory,
      real_doses = real_doses,
      d_star = d_star,
      target = target,
      alpha_mean = alpha_mean,
      alpha_sd = alpha_sd,
      beta_mean = beta_mean,
      beta_sd = beta_sd,
      tite = TRUE,
      ...
    )
  )
}

#' @importFrom trialr stan_nbg
trialr_nbg_selector <- function(parent_selector = NULL, outcomes, real_doses,
                                d_star, target,
                                alpha_mean, alpha_sd,
                                beta_mean, beta_sd,
                                tite = FALSE,
                                ...) {

  if(is.character(outcomes)) {
    if(tite) {
      stop(paste0("Provide outcomes in a data-frame for TITE-CRM with columns ",
                  "dose, tox and weight"))
    }
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  if(nrow(df) > 0) {
    # Checks
    if(max(df$dose) > length(real_doses)) {
      stop('trialr_crm_selector - maximum dose given exceeds number of doses.')
    }

    if(tite) {
      weights <- df$weight
    } else {
      weights <- rep(1, nrow(df))
    }

    x <-stan_nbg(outcome_str = NULL,
                 real_doses = real_doses,
                 d_star = d_star,
                 target = target,
                 alpha_mean = alpha_mean, alpha_sd = alpha_sd,
                 beta_mean = beta_mean, beta_sd = beta_sd,
                 doses_given = df$dose,
                 tox = df$tox %>% as.integer(),
                 weights = weights,
                 refresh = 0,
                 # Discard warmup & retain critical variables to save memory
                 save_warmup = FALSE,
                 pars = c('alpha', 'beta', 'prob_tox'),
                 ...)
  } else {
    d <- log(real_doses / d_star)
    prob_tox_sample <- inv.logit(
      rnorm(100, mean = alpha_mean, alpha_sd) +
        matrix(rnorm(100, mean = beta_mean, beta_sd), ncol = 1) %*%
        matrix(d, nrow = 1)
    )

    x <- list(
      doses = integer(length = 0),
      tox = integer(length = 0),
      recommended_dose = 1,
      prob_tox = apply(prob_tox_sample, 2, mean),
      median_prob_tox = apply(prob_tox_sample, 2, median)
    )
  }

  l <- list(
    parent = parent_selector,
    cohort = df$cohort,
    outcomes = outcomes,
    real_doses = real_doses,
    target = target,
    tite = tite,
    trialr_fit = x
  )

  class(l) = c('trialr_nbg_selector', 'tox_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.trialr_nbg_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    real_doses = selector_factory$real_doses,
    d_star = selector_factory$d_star,
    target = selector_factory$target,
    alpha_mean = selector_factory$alpha_mean,
    alpha_sd = selector_factory$alpha_sd,
    beta_mean = selector_factory$beta_mean,
    beta_sd = selector_factory$beta_sd,
    tite = selector_factory$tite
  )
  args <- append(args, selector_factory$extra_args)
  do.call(trialr_nbg_selector, args = args)
}

#' @export
simulation_function.trialr_nbg_selector_factory <- function(selector_factory) {
  if(selector_factory$tite) {
    return(phase1_tite_sim)
  } else {
    return(phase1_sim)
  }
}


# Selector interface

#' @export
tox_target.trialr_nbg_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.trialr_nbg_selector <- function(x, ...) {
  return(length(x$trialr_fit$doses))
}

#' @export
cohort.trialr_nbg_selector <- function(x, ...) {
  return(x$cohort)
}

#' @export
doses_given.trialr_nbg_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$doses))
}

#' @export
tox.trialr_nbg_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$tox))
}

#' @export
weight.trialr_nbg_selector <- function(x, ...) {
  if(x$tite) {
    return(x$trialr_fit$weights)
  } else {
    return(rep(1, num_patients(x)))
  }
}

#' @export
num_doses.trialr_nbg_selector <- function(x, ...) {
  return(length(x$trialr_fit$prob_tox))
}

#' @export
recommended_dose.trialr_nbg_selector <- function(x, ...) {
  if(!is.null(x$parent)) {
    parent_dose <- recommended_dose(x$parent)
    parent_cont <- continue(x$parent)
    if(parent_cont & !is.na(parent_dose)) {
      return(parent_dose)
    }
  }

  # By default:
  return(as.integer(x$trialr_fit$recommended_dose))
}

#' @export
continue.trialr_nbg_selector <- function(x, ...) {
  # This model in isolation offers no methods for stopping but those are
  # provided by other classes in this package.
  # In the daisychain of selectors, this class is resumptive, meaning it will
  # continue with dose-selection after its optional parent, where present, has
  # opted to not continue.
  # Thus, this class always opts to continue:
  return(TRUE)
}

#' @importFrom purrr map_int
#' @importFrom stats quantile
#' @export
tox_at_dose.trialr_nbg_selector <- function(x, ...) {
  dose_indices <- 1:(num_doses(x))
  tox_seen <- tox(x)
  map_int(dose_indices, ~ sum(tox_seen[doses_given(x) == .x]))
}

#' @export
mean_prob_tox.trialr_nbg_selector <- function(x, ...) {
  return(x$trialr_fit$prob_tox)
}

#' @export
#' @importFrom stats median
median_prob_tox.trialr_nbg_selector <- function(x, ...) {
  return(prob_tox_quantile(x, p = 0.5))
}

#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
prob_tox_quantile.trialr_nbg_selector <- function(x, p, ...) {
  if(num_patients(x) <= 0) {
    return(as.numeric(rep(NA, num_doses(x))))
  } else {
    .draw <- NULL
    prob_tox_samples(x) %>%
      select(-.draw) %>%
      apply(2, quantile, probs = p) %>%
      as.numeric()
  }
}

#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
prob_tox_exceeds.trialr_nbg_selector <- function(x, threshold, ...) {

  if(num_patients(x) <= 0) {
    return(as.numeric(rep(NA, num_doses(x))))
  } else {
    .draw <- NULL
    (prob_tox_samples(x) %>%
       select(-.draw) > threshold) %>%
      apply(2, mean) %>%
      as.numeric()
  }
}

#' @export
supports_sampling.trialr_nbg_selector <- function(x, ...) {
  return(TRUE)
}

#' @export
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr select everything mutate as_tibble
prob_tox_samples.trialr_nbg_selector <- function(x, tall = FALSE,...) {
  if(num_patients(x) > 0) {
    df <- x$trialr_fit %>%
      as.data.frame(pars = 'prob_tox')
  } else {
    df <- matrix(ncol = num_doses(x), nrow = 0) %>%
      as.data.frame()
  }

  colnames(df) <- as.character(dose_indices(x))
  . <- .draw <- NULL
  df <- df %>%
    mutate(.draw = row.names(.)) %>%
    select(.draw, everything()) %>%
    as_tibble()

  if(tall) {
    dose <- prob_tox <- .draw <- NULL
    df %>%
      gather(dose, prob_tox, -.draw)
  } else {
    return(df)
  }
}

#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select everything all_of
summary.trialr_nbg_selector <- function(object, ...) {
  Dose <- N <- Tox <- EmpiricToxRate <- RealDose <- NULL
  summary.selector(object) %>%
    mutate(RealDose = c(NA, object$real_doses)) %>%
    select(
      all_of(Dose),
      all_of(N),
      all_of(Tox),
      all_of(EmpiricToxRate),
      all_of(RealDose),
      everything()
    )
}
