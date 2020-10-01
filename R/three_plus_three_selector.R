
#' Get an object to fit the 3+3 model.
#'
#' @param num_doses Number of doses under investigation.
#' @param allow_deescalate TRUE to allow de-escalation, as described by Korn et
#' al. Default is FALSE.
#' @param ... Extra args are not currently used.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' 3+3 model to outcomes.
#'
#' @export
#'
#' @examples
#' model <- get_three_plus_three(num_doses = 5)
#'
#' fit1 <- model %>% fit('1NNN 2NTN')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 <- model %>% fit('1NNN 2NTN 2NNT')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' @references
#' Storer BE. Design and Analysis of Phase I Clinical Trials. Biometrics.
#' 1989;45(3):925-937. doi:10.2307/2531693
#'
#' Korn EL, Midthune D, Chen TT, Rubinstein LV, Christian MC, Simon RM.
#' A comparison of two phase I trial designs. Statistics in Medicine.
#' 1994;13(18):1799-1806. doi:10.1002/sim.4780131802
get_three_plus_three <- function(num_doses, allow_deescalate = FALSE, ...) {

  x <- list(
    num_doses = num_doses,
    allow_deescalate = allow_deescalate,
    extra_args = list(...)
  )

  class(x) <- c('three_plus_three_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

three_plus_three_selector <- function(outcomes,
                                      num_doses,
                                      allow_deescalate = FALSE,
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
      stop(
        paste0(
          'three_plus_three_selector - ',
          'maximum dose given exceeds number of doses.'
        ))
    }
  }

  three_plus_three_fit <- three_plus_three(outcomes = outcomes,
                                           num_doses = num_doses,
                                           allow_deescalate = allow_deescalate)

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    num_doses = as.integer(num_doses),
    allow_deescalate = allow_deescalate,
    df = df,
    df_c = df_c,
    three_plus_three_fit = three_plus_three_fit
  )

  class(l) = c('three_plus_three_selector', 'tox_selector', 'selector')
  l
}


# Factory interface

#' @export
fit.three_plus_three_selector_factory <- function(selector_factory, outcomes,
                                                  ...) {

  args <- list(
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    allow_deescalate = selector_factory$allow_deescalate
  )
  args <- append(args, selector_factory$extra_args)
  do.call(three_plus_three_selector, args = args)
}

# Selector interface

#' @export
num_patients.three_plus_three_selector <- function(x, ...) {
  return(length(x$df$dose))
}

#' @export
cohort.three_plus_three_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.three_plus_three_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.three_plus_three_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
num_doses.three_plus_three_selector <- function(x, ...) {
  return(x$num_doses)
}

#' @export
recommended_dose.three_plus_three_selector <- function(x, warn = TRUE,
                                                       ...) {
  return(x$three_plus_three_fit$recommended_dose)
}

#' @export
continue.three_plus_three_selector <- function(x, ...) {
  return(x$three_plus_three_fit$continue)
}

#' @export
tox_at_dose.three_plus_three_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
mean_prob_tox.three_plus_three_selector <- function(x, ...) {
  # message('Note that 3+3 does not estimate mean_prob_tox.')
  as.numeric(rep(NA, num_doses(x)))
}

#' @export
median_prob_tox.three_plus_three_selector <- function(x, ...) {
  # message('Note that 3+3 does not estimate median_prob_tox.')
  as.numeric(rep(NA, num_doses(x)))
}

#' @export
dose_admissible.three_plus_three_selector <- function(x, ...) {
  # Reject doses with at least 2 tox, and all higher doses
  reject <- x$df_c$tox >= 2
  cum_reject <- cumsum(reject) >= 1
  return(!cum_reject)
}

#' @export
prob_tox_quantile.three_plus_three_selector <- function(x, p, ...) {
  # message('Note that 3+3 does not estimate prob_tox_quantile.')
  as.numeric(rep(NA, num_doses(x)))
}

#' @export
prob_tox_exceeds.three_plus_three_selector <- function(x, threshold,
                                                       iter = 1000, ...) {
  # message('Note that 3+3 does not estimate prob_tox_exceeds.')
  as.numeric(rep(NA, num_doses(x)))
}

#' @export
supports_sampling.three_plus_three_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.three_plus_three_selector <- function(
  x, tall = FALSE, ...) {
  stop('three_plus_three_selector does not support sampling.')
}

#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select
summary.three_plus_three_selector <- function(object, ...) {
  MeanProbTox <- MedianProbTox <- NULL
  summary.selector(object) %>% select(-MeanProbTox, -MedianProbTox)
}

