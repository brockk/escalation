
#' Expand the cohort of the last given dose to at least n patients
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param n Continue at current dose until at least n patients are evaluated.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' # TODO
expand_last_dose_to_cohort <- function(parent_selector_factory, n) {

  x <- list(
    parent = parent_selector_factory,
    n = n
  )
  class(x) <- c('expand_last_dose_to_cohort_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

expand_last_dose_to_cohort_selector <- function(parent_selector, n) {

  l <- list(
    parent = parent_selector,
    n = n
  )

  class(l) = c('expand_last_dose_to_cohort_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.expand_last_dose_to_cohort_selector_factory <- function(selector_factory,
                                                            outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(expand_last_dose_to_cohort_selector(parent_selector = parent_selector,
                                             n = selector_factory$n))
}

# Selector interface

#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @export
continue.expand_last_dose_to_cohort_selector <- function(x, ...) {
  parent_cont <- continue(x$parent)
  if(num_patients(x) > 0 & !parent_cont) {
    last_dose <- tail(doses_given(x), 1)
    # Override to achieve the target cohort size at the last dose
    if(n_at_dose(x, dose = last_dose) < x$n) {
      return(TRUE)
    }
  }

  # By default:
  return(continue(x$parent))
}

#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @export
recommended_dose.expand_last_dose_to_cohort_selector <- function(x, ...) {
  parent_cont <- continue(x$parent)
  if(num_patients(x) > 0 & !parent_cont) {
    last_dose <- tail(doses_given(x), 1)
    # Override to achieve the target cohort size at the last dose
    if(n_at_dose(x, dose = last_dose) < x$n) {
      return(last_dose)
    }
  }

  # By default:
  return(recommended_dose(x$parent))
}
