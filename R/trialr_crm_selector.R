
#' Get an object to fit the CRM model using the trialr package.
#'
#' @description
#' This function returns an object that can be used to fit a CRM model using
#' methods provided by the trialr package.
#'
#' Dose selectors are designed to be daisy-chained together to achieve different
#' behaviours. This class is a **resumptive** selector, meaning it carries on
#' when the previous dose selector, where present, has elected not to continue.
#' For example, this allows instances of this class to be preceded by a selector
#' that follows a fixed path in an initial escalation plan, such as that
#' provided by \code{\link{follow_path}}. In this example, when the observed
#' trial outcomes deviate from that initial plan, the selector following the
#' fixed path elects not to continue and responsibility passes to this class.
#' See Examples.
#'
#' The time-to-event variant, TITE-CRM, is used when you specify
#' \code{tite = TRUE}. This weights the observations to allow dose-selections
#' based on partially observed outcomes.
#'
#' @param parent_selector_factory optional object of type
#' \code{\link{selector_factory}} that is in charge of dose selection before
#' this class gets involved. Leave as NULL to just use CRM from the start.
#' @param skeleton Dose-toxicity skeleton, a non-decreasing vector of
#' probabilities.
#' @param target We seek a dose with this probability of toxicity.
#' @param model character string identifying which model form to use. Options
#' include empiric, logistic, logistic2. The model form chosen determines which
#' prior hyperparameters are required. See \code{\link[trialr]{stan_crm}}
#' for more details.
#' @param tite FALSE to use regular CRM; TRUE to use TITE-CRM. See Description.
#' @param ... Extra args are passed to \code{\link[trialr]{stan_crm}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' CRM model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' # The model to use must be specified in trialr:
#' model1 <- get_trialr_crm(skeleton = skeleton, target = target,
#'                          model = 'empiric', beta_sd = 1.34)
#' # Refer to the trialr documentation for more details on model forms.
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' # But we can provide extra args to trialr that are than passed onwards to
#' # the call to trialr::stan_crm to override the defaults.
#' # For example, if we want the one-parameter logistic model, we run:
#' model2 <- get_trialr_crm(skeleton = skeleton, target = target,
#'                          model = 'logistic', a0 = 3,
#'                          beta_mean = 0, beta_sd = 1)
#' model2 %>% fit(outcomes) %>% recommended_dose()
#' # And, if we want the two-parameter logistic model, we run:
#' model3 <- get_trialr_crm(skeleton = skeleton, target = target,
#'                          model = 'logistic2',
#'                          alpha_mean = 0, alpha_sd = 2,
#'                          beta_mean = 0, beta_sd = 1)
#' model3 %>% fit(outcomes) %>% recommended_dose()
#'
#' # We can use an initial dose-escalation plan, a pre-specified path that
#' # should be followed until trial outcomes deviate, at which point the CRM
#' # model takes over. For instance, if we want to use two patients at each of
#' # the first three doses in the absence of toxicity, irrespective the model's
#' # advice, we would run:
#' model1 <- follow_path('1NN 2NN 3NN') %>%
#'   get_trialr_crm(skeleton = skeleton, target = target, model = 'empiric',
#'                  beta_sd = 1.34)
#'
#' # If outcomes match the desired path, the path is followed further:
#' model1 %>% fit('1NN 2N') %>% recommended_dose()
#'
#' # But when the outcomes diverge:
#' model1 %>% fit('1NN 2T') %>% recommended_dose()
#'
#' # Or the pre-specified path comes to an end:
#' model1 %>% fit('1NN 2NN 3NN') %>% recommended_dose()
#' # ...the CRM model takes over.
#'
#' @references
#' Kristian Brock (2020). trialr: Clinical Trial Designs in 'rstan'.
#' R package version 0.1.5. https://github.com/brockk/trialr
#'
#' Kristian Brock (2019). trialr: Bayesian Clinical Trial Designs in R and Stan.
#' arXiv preprint arXiv:1907.00161.
get_trialr_crm <- function(parent_selector_factory = NULL, skeleton, target,
                           model, tite = FALSE, ...) {

  x <- list(
    parent_selector_factory = parent_selector_factory,
    skeleton = skeleton,
    target = target,
    model = model,
    tite = tite,
    extra_args = list(...)
  )

  class(x) <- c('trialr_crm_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

#' Get an object to fit the TITE-CRM model using the trialr package.
#'
#' @details
#' This function is a short-cut to \code{get_trialr_crm(tite = TRUE)}. See
#' \code{\link{get_trialr_crm}} for full details.
#'
#' @inheritParams get_trialr_crm
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' CRM model to outcomes.
#'
#' @export
#'
#' @examples
#' # TODO
get_trialr_crm_tite <- function(parent_selector_factory = NULL, skeleton,
                                target, model, ...) {
  return(
    get_trialr_crm(
      parent_selector_factory = parent_selector_factory,
      skeleton = skeleton,
      target = target,
      model = model,
      tite = TRUE,
      ...
    )
  )
}

#' @importFrom trialr stan_crm
trialr_crm_selector <- function(parent_selector = NULL, outcomes, skeleton,
                                target, model, tite = FALSE, ...) {

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
    if(max(df$dose) > length(skeleton)) {
      stop('trialr_crm_selector - maximum dose given exceeds number of doses.')
    }

    if(tite) {
      weights <- df$weight
    } else {
      weights <- rep(1, nrow(df))
    }

    if(model %in% c('empiric', 'logistic', 'logistic_gamma')) {
      x <-stan_crm(outcome_str = NULL,
                   skeleton = skeleton,
                   target = target,
                   model = model,
                   doses_given = df$dose,
                   tox = df$tox %>% as.integer(),
                   weights = weights,
                   refresh = 0,
                   # Discard warmup & retain critical variables to save memory
                   save_warmup = FALSE,
                   pars = c('beta', 'prob_tox'),
                   ...)
    } else if(model == 'logistic2') {
      x <-stan_crm(outcome_str = NULL,
                   skeleton = skeleton,
                   target = target,
                   model = model,
                   doses_given = df$dose,
                   tox = df$tox %>% as.integer(),
                   weights = weights,
                   refresh = 0,
                   # Discard warmup & retain critical variables to save memory
                   save_warmup = FALSE,
                   pars = c('alpha', 'beta', 'prob_tox'),
                   ...)
    } else {
      warning(paste0('Could not refine variable set for ', model, ' model. ',
                     'The returned object could be larger than is ideal.'))
      x <-stan_crm(outcome_str = NULL,
                   skeleton = skeleton,
                   target = target,
                   model = model,
                   doses_given = df$dose,
                   tox = df$tox %>% as.integer(),
                   weights = weights,
                   refresh = 0,
                   # Discard warmup & retain critical variables to save memory
                   save_warmup = FALSE,
                   ...)
    }

  } else {
    x <- list(
      doses = integer(length = 0),
      tox = integer(length = 0),
      recommended_dose = 1,
      prob_tox = skeleton,
      median_prob_tox = skeleton
      )
  }

  l <- list(
    parent = parent_selector,
    cohort = df$cohort,
    outcomes = outcomes,
    skeleton = skeleton,
    target = target,
    tite = tite,
    trialr_fit = x
  )

  class(l) = c('trialr_crm_selector', 'tox_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.trialr_crm_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    skeleton = selector_factory$skeleton,
    target = selector_factory$target,
    model = selector_factory$model,
    tite = selector_factory$tite
  )
  args <- append(args, selector_factory$extra_args)
  do.call(trialr_crm_selector, args = args)
}

#' @export
simulation_function.trialr_crm_selector_factory <- function(selector_factory) {
  if(selector_factory$tite) {
    return(phase1_tite_sim)
  } else {
    return(phase1_sim)
  }
}


# Selector interface

#' @export
tox_target.trialr_crm_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.trialr_crm_selector <- function(x, ...) {
  return(length(x$trialr_fit$doses))
}

#' @export
cohort.trialr_crm_selector <- function(x, ...) {
  return(x$cohort)
}

#' @export
doses_given.trialr_crm_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$doses))
}

#' @export
tox.trialr_crm_selector <- function(x, ...) {
  return(as.integer(x$trialr_fit$tox))
}

#' @export
weight.trialr_crm_selector <- function(x, ...) {
  if(x$tite) {
    return(x$trialr_fit$weights)
  } else {
    return(rep(1, num_patients(x)))
  }
}

#' @export
num_doses.trialr_crm_selector <- function(x, ...) {
  return(length(x$trialr_fit$prob_tox))
}

#' @export
recommended_dose.trialr_crm_selector <- function(x, ...) {
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
continue.trialr_crm_selector <- function(x, ...) {
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
tox_at_dose.trialr_crm_selector <- function(x, ...) {
  dose_indices <- 1:(num_doses(x))
  tox_seen <- tox(x)
  map_int(dose_indices, ~ sum(tox_seen[doses_given(x) == .x]))
}

#' @export
mean_prob_tox.trialr_crm_selector <- function(x, ...) {
  return(x$trialr_fit$prob_tox)
}

#' @export
#' @importFrom stats median
median_prob_tox.trialr_crm_selector <- function(x, ...) {
  return(prob_tox_quantile(x, p = 0.5))
}

#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom stats quantile
prob_tox_quantile.trialr_crm_selector <- function(x, p, ...) {
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
prob_tox_exceeds.trialr_crm_selector <- function(x, threshold, ...) {

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
supports_sampling.trialr_crm_selector <- function(x, ...) {
  return(TRUE)
}

#' @export
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr select everything mutate as_tibble
prob_tox_samples.trialr_crm_selector <- function(x, tall = FALSE,...) {
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
summary.trialr_crm_selector <- function(object, ...) {
  Dose <- N <- Tox <- EmpiricToxRate <- Skeleton <- NULL
  summary.selector(object) %>%
    mutate(Skeleton = c(NA, object$skeleton)) %>%
    select(
      all_of(Dose),
      all_of(N),
      all_of(Tox),
      all_of(EmpiricToxRate),
      all_of(Skeleton),
      everything()
    )
}
