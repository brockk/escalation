
#' Get an object to fit a dose-selector that randomly selects doses.
#'
#' Get an object to fit a dose-selector that randomly selects doses. Whilst this
#' design is unlikely to pass the ethical hurdles when investigating truly
#' experimental treatments, this class is useful for illustrating methods and
#' can be useful for benchmarking.
#'
#' @param parent_selector_factory optional object of type
#' \code{\link{selector_factory}} that is in charge of dose selection before
#' this class gets involved. Leave as NULL to just select random doses from the
#' start.
#' @param prob_select vector of probabilities, the probability of selecting
#' dose 1...n
#' @param supports_efficacy TRUE to monitor toxicity and efficacy outcomes;
#' FALSE (by default) to just monitor toxicity outcomes.
#' @param ... Extra args are ignored.
#'
#' @return an object of type \code{\link{selector_factory}}.
#' @export
#'
#' @examples
#' prob_select = c(0.1, 0.3, 0.5, 0.07, 0.03)
#' model <- get_random_selector(prob_select = prob_select)
#' fit <- model %>% fit('1NTN')
#' fit %>% recommended_dose() # This is random
#' # We could also precede this selector with a set path:
#' model <- follow_path('1NN 2NN 3NN') %>%
#'   get_random_selector(prob_select = prob_select)
#' fit <- model %>% fit('1NN')
#' fit %>% recommended_dose() # This is not-random; it comes from the path.
#' fit <- model %>% fit('1NN 2NT')
#' fit %>% recommended_dose() # This is random; the path is discarded.
get_random_selector <- function(parent_selector_factory = NULL, prob_select,
                                supports_efficacy = FALSE, ...) {
  x <- list(
    parent_selector_factory = parent_selector_factory,
    prob_select = prob_select,
    supports_efficacy = supports_efficacy,
    extra_args = list(...)
  )

  class(x) <- c('random_selector_factory',
                'eff_tox_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

#' @export
simulation_function.random_selector_factory <- function(selector_factory) {
  if(selector_factory$supports_efficacy)
    return(phase1_2_sim)
  else
    return(phase1_sim)
}

#' @export
dose_paths_function.random_selector_factory <- function(selector_factory) {
  if(selector_factory$supports_efficacy)
    return(phase1_2_dose_paths)
  else
    return(phase1_dose_paths)
}

random_selector <- function(parent_selector = NULL, outcomes, prob_select,
                            supports_efficacy, ...) {

  if(is.character(outcomes)) {
    if(supports_efficacy) {
      df <- parse_phase1_2_outcomes(outcomes, as_list = FALSE)
    } else {
      df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
    }
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
    if('eff' %in% colnames(df) & !supports_efficacy) {
      message(paste("Setting 'supports_efficacy = TRUE' because column 'eff'",
      "exists in outcomes data-frame. Drop the column to suppress this",
      "behaviour."))
      supports_efficacy = TRUE
    }
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  df_c <- model_frame_to_counts(df, num_doses = length(prob_select))

  if(nrow(df) > 0) {
    # Checks
    if(max(df$dose) > length(prob_select)) {
      stop('random_selector - maximum dose given exceeds number of doses.')
    }
  }

  l <- list(
    parent = parent_selector,
    prob_select = prob_select,
    supports_efficacy = supports_efficacy,
    df = df,
    df_c = df_c
  )

  class(l) = c('random_selector',
               'eff_tox_selector',
               'tox_selector',
               'selector')
  l
}

# Factory interface

#' @export
fit.random_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    prob_select = selector_factory$prob_select,
    supports_efficacy = selector_factory$supports_efficacy
  )
  args <- append(args, selector_factory$extra_args)
  do.call(random_selector, args = args)
}


# Selector interface

#' @export
num_patients.random_selector <- function(x, ...) {
  return(nrow(x$df))
}

#' @export
cohort.random_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.random_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.random_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
eff.random_selector <- function(x, ...) {
  if(x$supports_efficacy) {
    return(x$df$eff)
  } else {
    return(as.integer(rep(NA, num_patients(x))))
  }
}

#' @export
num_doses.random_selector <- function(x, ...) {
  return(length(x$prob_select))
}

#' @export
recommended_dose.random_selector <- function(x, ...) {
  if(!is.null(x$parent)) {
    parent_dose <- recommended_dose(x$parent)
    parent_cont <- continue(x$parent)
    if(parent_cont & !is.na(parent_dose)) {
      return(parent_dose)
    }
  }

  # By default:
  sample(x = dose_indices(x), size = 1, replace = FALSE, prob = x$prob_select)
}

#' @export
continue.random_selector <- function(x, ...) {
  return(TRUE)
}

#' @export
tox_at_dose.random_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
eff_at_dose.random_selector <- function(x, ...) {
  if(x$supports_efficacy) {
    return(x$df_c$eff)
  } else {
    return(as.integer(rep(NA, num_doses(x))))
  }
}

#' @export
mean_prob_tox.random_selector <- function(x, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
median_prob_tox.random_selector <- function(x, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
mean_prob_eff.random_selector <- function(x, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
median_prob_eff.random_selector <- function(x, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
prob_tox_quantile.random_selector <- function(x, p, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
prob_tox_exceeds.random_selector <- function(x, threshold, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
prob_eff_quantile.random_selector <- function(x, p, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
prob_eff_exceeds.random_selector <- function(x, threshold, ...) {
  return(as.numeric(rep(NA, num_doses(x))))
}

#' @export
supports_sampling.random_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.random_selector <- function(x,...) {
  stop('random_selector does not support sampling.')
}
