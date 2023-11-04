
#' Get an object to fit the EffTox model using the trialr package.
#'
#' @description
#' This function returns an object that can be used to fit the EffTox model for
#' phase I/II dose-finding using methods provided by the trialr package.
#'
#' @param parent_selector_factory optional object of type
#' \code{\link{selector_factory}} that is in charge of dose selection before
#' this class gets involved. Leave as NULL to just use EffTox from the start.
#' @param real_doses A vector of numbers, the doses under investigation. They
#' should be ordered from lowest to highest and be in consistent units.
#' E.g. to conduct a dose-finding trial of doses 10mg, 20mg and 50mg, use
#' c(10, 20, 50).
#' @param efficacy_hurdle Minimum acceptable efficacy probability.
#' A number between 0 and 1.
#' @param toxicity_hurdle Maximum acceptable toxicity probability.
#' A number between 0 and 1.
#' @param p_e Certainty required to infer a dose is acceptable with regards to
#' being probably efficacious; a number between 0 and 1.
#' @param p_t Certainty required to infer a dose is acceptable with regards to
#' being probably tolerable; a number between 0 and 1.
#' @param eff0 Efficacy probability required when toxicity is impossible;
#' a number between 0 and 1 (see Details).
#' @param tox1 Toxicity probability permitted when efficacy is guaranteed;
#' a number between 0 and 1 (see Details).
#' @param eff_star Efficacy probability of an equi-utility third point (see
#' Details).
#' @param tox_star Toxicity probability of an equi-utility third point (see
#' Details).
#' @param priors instance of class \code{\link{trialr}{efftox_priors}}, the
#' hyperparameters for normal priors on the six model parameters.
#' @param ... Extra args are passed to \code{\link[trialr]{stan_efftox}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' EffTox model to outcomes.
#'
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#'
#' @references
#' Thall, P., & Cook, J. (2004). Dose-Finding Based on Efficacy-Toxicity
#' Trade-Offs. Biometrics, 60(3), 684-693.
#' https://doi.org/10.1111/j.0006-341X.2004.00218.x
#'
#' Thall, P., Herrick, R., Nguyen, H., Venier, J., & Norris, J. (2014).
#' Effective sample size for computing prior hyperparameters in Bayesian
#' phase I-II dose-finding. Clinical Trials, 11(6), 657-666.
#' https://doi.org/10.1177/1740774514547397
#'
#' Brock, K. (2020). trialr: Clinical Trial Designs in 'rstan'.
#' R package version 0.1.5. https://github.com/brockk/trialr
#'
#' Brock, K. (2019). trialr: Bayesian Clinical Trial Designs in R and Stan.
#' arXiv preprint arXiv:1907.00161.
get_trialr_efftox <- function(parent_selector_factory = NULL,
                              real_doses, efficacy_hurdle, toxicity_hurdle,
                              p_e, p_t, eff0, tox1, eff_star, tox_star,
                              priors,
                              ...) {

  x <- list(
    parent_selector_factory = parent_selector_factory,
    real_doses = real_doses,
    efficacy_hurdle = efficacy_hurdle,
    toxicity_hurdle = toxicity_hurdle,
    p_e = p_e,
    p_t = p_t,
    eff0 = eff0,
    tox1 = tox1,
    eff_star = eff_star,
    tox_star = tox_star,
    priors = priors,
    extra_args = list(...)
  )

  class(x) <- c('trialr_efftox_selector_factory',
                'eff_tox_selector_factory',
                'selector_factory')
  return(x)
}

#' @importFrom trialr stan_efftox
trialr_efftox_selector <- function(parent_selector = NULL, outcomes,
                                   real_doses, efficacy_hurdle, toxicity_hurdle,
                                   p_e, p_t, eff0, tox1, eff_star, tox_star,
                                   priors,
                                   ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_2_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  if(nrow(df) > 0) {
    # Checks
    if(max(df$dose) > length(real_doses)) {
      stop('trialr_efftox_selector - maximum dose given exceeds number of doses.')
    }

    x <-stan_efftox(outcome_str = NULL,
                    real_doses = real_doses,
                    efficacy_hurdle = efficacy_hurdle,
                    toxicity_hurdle = toxicity_hurdle,
                    p_e = p_e,
                    p_t = p_t,
                    eff0 = eff0,
                    tox1 = tox1,
                    eff_star = eff_star,
                    tox_star = tox_star,
                    priors = priors,
                    doses_given = df$dose,
                    tox = df$tox %>% as.integer(),
                    eff = df$eff %>% as.integer(),
                    refresh = 0,
                    # Discard warmup & retain critical variables to save memory
                    save_warmup = FALSE,
                    pars = c('alpha', 'beta', 'gamma', 'zeta', 'eta', 'psi',
                             'prob_eff', 'prob_tox', 'utility'),
                    ...)

  } else {
    x <- list(
      doses = integer(length = 0),
      eff = integer(length = 0),
      tox = integer(length = 0),
      recommended_dose = 1,
      prob_tox = rep(NA, length(real_doses)),
      median_prob_tox = rep(NA, length(real_doses)),
      prob_eff = rep(NA, length(real_doses)),
      median_prob_eff = rep(NA, length(real_doses))
    )
  }

  l <- list(
    parent = parent_selector,
    cohort = df$cohort,
    eff_limit = efficacy_hurdle,
    tox_limit = toxicity_hurdle,
    trialr_fit = x
  )

  class(l) = c('trialr_efftox_selector', 'eff_tox_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.trialr_efftox_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    real_doses = selector_factory$real_doses,
    efficacy_hurdle = selector_factory$efficacy_hurdle,
    toxicity_hurdle = selector_factory$toxicity_hurdle,
    p_e = selector_factory$p_e,
    p_t = selector_factory$p_t,
    eff0 = selector_factory$eff0,
    tox1 = selector_factory$tox1,
    eff_star = selector_factory$eff_star,
    tox_star = selector_factory$tox_star,
    priors = selector_factory$priors
  )
  args <- append(args, selector_factory$extra_args)
  do.call(trialr_efftox_selector, args = args)
}

# Selector interface

#' @export
tox_limit.trialr_efftox_selector <- function(x, ...) {
  return(x$tox_limit)
}

#' @export
eff_limit.trialr_efftox_selector <- function(x, ...) {
  return(x$eff_limit)
}

#' @export
num_patients.trialr_efftox_selector <- function(x, ...) {
  return(length(x$trialr_fit$doses))
}

#' @export
cohort.trialr_efftox_selector <- function(x, ...) {
  return(x$cohort)
}

#' @export
doses_given.trialr_efftox_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$doses))
}

#' @export
tox.trialr_efftox_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$tox))
}

#' @export
eff.trialr_efftox_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$eff))
}

#' @export
num_doses.trialr_efftox_selector <- function(x, ...) {
  return(length(x$trialr_fit$prob_tox))
}

#' @export
recommended_dose.trialr_efftox_selector <- function(x, ...) {
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
continue.trialr_efftox_selector <- function(x, ...) {
  # Plain CRM offers no methods for stopping but those are provided by other
  # classes in this package.
  # In the daisychain of selectors, this class is resumptive, meaning it will
  # continue with dose-selection after its optional parent, where present, has
  # opted to not continue.
  # Thus, this class always opts to continue:
  return(TRUE)
}

#' @importFrom purrr map_int
#' @export
tox_at_dose.trialr_efftox_selector <- function(x, ...) {
  dose_indices <- 1:(num_doses(x))
  tox_seen <- tox(x)
  map_int(dose_indices, ~ sum(tox_seen[doses_given(x) == .x]))
}

#' @export
mean_prob_tox.trialr_efftox_selector <- function(x, ...) {
  if(num_patients(x) > 0)
    return(x$trialr_fit$prob_tox)
  else
    return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
#' @importFrom stats median
median_prob_tox.trialr_efftox_selector <- function(x, ...) {
  return(prob_tox_quantile(x, p = 0.5))
}

#' @export
mean_prob_eff.trialr_efftox_selector <- function(x, ...) {
  if(num_patients(x) > 0)
    return(x$trialr_fit$prob_eff)
  else
    return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
#' @importFrom stats median
median_prob_eff.trialr_efftox_selector <- function(x, ...) {
  return(prob_eff_quantile(x, p = 0.5))
}

#' @export
dose_admissible.trialr_efftox_selector <- function(x, ...) {
  if(num_patients(x) > 0)
    return(x$trialr_fit$acceptable)
  else
    return(rep(TRUE, num_doses(x)))
}


#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom stats quantile
prob_tox_quantile.trialr_efftox_selector <- function(x, p, ...) {
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
prob_tox_exceeds.trialr_efftox_selector <- function(x, threshold, ...) {

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
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom stats quantile
prob_eff_quantile.trialr_efftox_selector <- function(x, p, ...) {
  if(num_patients(x) <= 0) {
    return(as.numeric(rep(NA, num_doses(x))))
  } else {
    .draw <- NULL
    prob_eff_samples(x) %>%
      select(-.draw) %>%
      apply(2, quantile, probs = p) %>%
      as.numeric()
  }
}

#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
prob_eff_exceeds.trialr_efftox_selector <- function(x, threshold, ...) {

  if(num_patients(x) <= 0) {
    return(as.numeric(rep(NA, num_doses(x))))
  } else {
    .draw <- NULL
    (prob_eff_samples(x) %>%
        select(-.draw) > threshold) %>%
      apply(2, mean) %>%
      as.numeric()
  }
}

#' @export
supports_sampling.trialr_efftox_selector <- function(x, ...) {
  return(TRUE)
}

#' @export
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr select everything mutate as_tibble
prob_tox_samples.trialr_efftox_selector <- function(x, tall = FALSE,...) {
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
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr select everything mutate as_tibble
prob_eff_samples.trialr_efftox_selector <- function(x, tall = FALSE,...) {
  if(num_patients(x) > 0) {
    df <- x$trialr_fit %>%
      as.data.frame(pars = 'prob_eff')
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
    dose <- prob_eff <- .draw <- NULL
    df %>%
      gather(dose, prob_eff, -.draw)
  } else {
    return(df)
  }
}

#' @export
utility.trialr_efftox_selector <- function(x, ...) {
  return(x$trialr_fit$utility)
}

