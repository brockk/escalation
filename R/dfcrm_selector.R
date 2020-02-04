
#' Get an object to fit the CRM model using the dfcrm package.
#'
#' @param skeleton Dose-toxicity skeleton, a non-decreasing vector of
#' probabilities.
#' @param target We seek a dose with this probability of toxicity.
#' @param ... Extra args are passed to \code{\link[dfcrm]{crm}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' CRM model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton, target)
#'
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' model2 <- get_dfcrm(skeleton, target, model = 'logistic')
#' model2 %>% fit(outcomes) %>% recommended_dose()
#'
#' @references
#' Ken Cheung (2019). dfcrm: Dose-Finding by the Continual Reassessment Method.
#' R package version 0.2-2.1. https://CRAN.R-project.org/package=dfcrm
#'
#' O’Quigley J, Pepe M, Fisher L. Continual reassessment method: a practical
#' design for phase 1 clinical trials in cancer. Biometrics. 1990;46(1):33-48.
#' doi:10.2307/2531628
get_dfcrm <- function(skeleton, target, ...) {

  x <- list(
    skeleton = skeleton,
    target = target,
    extra_args = list(...)
  )

  class(x) <- c('selector_factory',
                'tox_selector_factory',
                'dfcrm_selector_factory')
  return(x)
}

#' @importFrom dfcrm crm
dfcrm_selector <- function(outcomes, skeleton, target, ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  if(nrow(df) > 0) {
    # Checks
    if(max(df$dose) > length(skeleton)) {
      stop('dfcrm_selector - maximum dose given exceeds number of doses.')
    }
    x <- crm(prior = skeleton,
             target = target,
             tox = df$tox %>% as.integer(),
             level = df$dose,
             var.est = TRUE,
             ...)
  } else {
    x <- list(
      level = integer(length = 0),
      tox = integer(length = 0),
      mtd = 1,
      ptox = skeleton)
  }

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    skeleton = skeleton,
    dfcrm_fit = x
  )

  class(l) = c('selector', 'tox_selector', 'dfcrm_selector')
  l
}

# Factory interface

#' @export
fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    skeleton = selector_factory$skeleton,
    target = selector_factory$target
  )
  args <- append(args, selector_factory$extra_args)
  do.call(dfcrm_selector, args = args)
}

# Selector interface

#' @importFrom magrittr %>%
#' @export
num_patients.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level %>% length)
}

#' @export
cohort.dfcrm_selector <- function(selector, ...) {
  return(selector$cohort)
}

#' @export
doses_given.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level)
}

#' @export
tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$tox)
}

#' @export
num_doses.dfcrm_selector <- function(selector, ...) {
  return(length(selector$skeleton))
}

#' @export
recommended_dose.dfcrm_selector <- function(selector, ...) {
  return(as.integer(selector$dfcrm_fit$mtd))
}

#' @export
continue.dfcrm_selector <- function(selector, ...) {
  return(TRUE)
}

#' @importFrom magrittr %>%
#' @importFrom purrr map_int
#' @export
n_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  map_int(dose_indices, ~ sum(selector %>% doses_given() == .x))
}

#' @importFrom magrittr %>%
#' @importFrom purrr map_int
#' @export
tox_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  tox_seen <- selector %>% tox()
  map_int(dose_indices,
                 ~ sum(tox_seen[selector %>% doses_given() == .x])
  )
}

#' @export
mean_prob_tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$ptox)
}

#' @export
median_prob_tox.dfcrm_selector <- function(selector, iter = 1000, ...) {
  if(num_patients(selector) <= 0) {
    return(rep(NA, num_doses(selector)))
  } else {
    prob_tox_sample <- get_posterior_prob_tox_sample(selector, iter)
    # Median(Prob(Tox) | data) is approximated by:
    apply(prob_tox_sample, 2, median)
  }
}

#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @importFrom gtools inv.logit
#' @export
prob_tox_exceeds.dfcrm_selector <- function(selector, threshold, iter = 1000,
                                            ...) {
  if(num_patients(selector) <= 0) {
    return(rep(NA, num_doses(selector)))
  } else {
    prob_tox_sample <- get_posterior_prob_tox_sample(selector, iter)
    # Prob(Prob(Tox) > threshold | data) is approximated by:
    colMeans(prob_tox_sample > threshold)
  }
}
