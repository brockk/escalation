
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
    num_doses = selector_factory$num_doses
  )
  args <- append(args, selector_factory$extra_args)
  do.call(three_plus_three_selector, args = args)
}

# Selector interface

#' @export
num_patients.three_plus_three_selector <- function(selector, ...) {
  return(length(selector$df$dose))
}

#' @export
cohort.three_plus_three_selector <- function(selector, ...) {
  return(selector$df$cohort)
}

#' @export
doses_given.three_plus_three_selector <- function(selector, ...) {
  return(selector$df$dose)
}

#' @export
tox.three_plus_three_selector <- function(selector, ...) {
  return(selector$df$tox)
}

#' @export
num_doses.three_plus_three_selector <- function(selector, ...) {
  return(selector$num_doses)
}

#' @export
recommended_dose.three_plus_three_selector <- function(selector, warn = TRUE,
                                                       ...) {
  return(selector$three_plus_three_fit$recommended_dose)
}

#' @export
continue.three_plus_three_selector <- function(selector, ...) {
  return(selector$three_plus_three_fit$continue)
}

#' @export
n_at_dose.three_plus_three_selector <- function(selector, ...) {
  return(selector$df_c$n)
}

#' @export
tox_at_dose.three_plus_three_selector <- function(selector, ...) {
  return(selector$df_c$tox)
}

#' @export
mean_prob_tox.three_plus_three_selector <- function(selector, ...) {
  # message('Note that 3+3 does not estimate mean_prob_tox.')
  as.numeric(rep(NA, num_doses(selector)))
}

#' @export
median_prob_tox.three_plus_three_selector <- function(selector, ...) {
  # message('Note that 3+3 does not estimate median_prob_tox.')
  as.numeric(rep(NA, num_doses(selector)))
}

#' @export
prob_tox_quantile.three_plus_three_selector <- function(selector, p, ...) {
  # message('Note that 3+3 does not estimate prob_tox_quantile.')
  as.numeric(rep(NA, num_doses(selector)))
}

#' @export
prob_tox_exceeds.three_plus_three_selector <- function(selector, threshold,
                                                       iter = 1000, ...) {
  # message('Note that 3+3 does not estimate prob_tox_exceeds.')
  as.numeric(rep(NA, num_doses(selector)))
}

#' @export
supports_sampling.three_plus_three_selector <- function(selector, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.three_plus_three_selector <- function(
  selector, tall = FALSE, ...) {
  stop('three_plus_three_selector does not support sampling.')
}
