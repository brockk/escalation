
#' Demand that a rescue dose is tried before stopping is permitted.
#'
#' This method continues a dose-finding trial until a safety dose has been given
#' to n patients. Once that condition is met, it delegates dose selelcting and
#' stopping responsibility to its parent dose selector, whatever that might be.
#' This class is greedy in that it meets its own needs before asking any other
#' selectors higher in the chain what they want. Thus, different behaviours may
#' be achieved by nesting dose selectors in different orders. See examples.
#'
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param n Continue at least until there are n at a dose.
#' @param dose an integer to identify the sought rescue dose-level.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#'
#' # This model will demand the lowest dose is tried in at least two patients
#' # before the trial is stopped for excess toxicity
#' model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8) %>%
#'   try_rescue_dose(dose = 1, n = 2)
#'
#' # In contrast, this model will stop for excess toxicity without trying dose 1
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.8)
#'
#' # For non-toxic outcomes, both designs will continue at sensible doses:
#' fit1 <- model1 %>% fit('2NNN')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 <- model2 %>% fit('2NNN')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' # For toxic outcomes, the design 1 will use dose 1 before stopping is allowed
#' fit1 <- model1 %>% fit('2TTT')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' # For toxic outcomes, however, design 2 will stop despite dose 1 being
#' # untested:
#' fit2 <- model2 %>% fit('2TTT')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' # After dose 1 is given the requisite number of times, dose recommendation
#' # and stopping revert to being determined by the underlying dose selector:
#' fit1 <- model1 %>% fit('2TTT 1T')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit1 <- model1 %>% fit('2TTT 1TT')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
try_rescue_dose <- function(parent_selector_factory, n, dose) {

  x <- list(
    parent = parent_selector_factory,
    n = n,
    dose = dose
  )
  class(x) <- c('try_rescue_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

try_rescue_dose_selector <- function(parent_selector, n, dose) {

  l <- list(
    parent = parent_selector,
    n = n,
    dose = dose
  )

  class(l) = c('try_rescue_dose_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.try_rescue_dose_selector_factory <- function(selector_factory,
                                                  outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(try_rescue_dose_selector(parent_selector = parent_selector,
                                  n = selector_factory$n,
                                  dose = selector_factory$dose))
}

# Selector interface

#' @importFrom magrittr %>%
#' @export
recommended_dose.try_rescue_dose_selector <- function(x, ...) {

  # This selector affects when a trial ends and which dose is selected but only
  # when the parent has selected no dose.
  parent_dose <- x$parent %>% recommended_dose()
  if(is.na(parent_dose)) {
    n_at_dose <- x %>% n_at_dose()
    if(x$dose >= 1 & x$dose <= x %>% num_doses()) {
      if(n_at_dose[x$dose] >= x$n) {
        return(x$parent %>% recommended_dose())
      } else {
        # Recommend rescue dose
        return(x$dose)
      }
    }

    # By default:
    return(x$parent %>% recommended_dose())
  } else {
    return(x$parent %>% recommended_dose())
  }
}

#' @importFrom magrittr %>%
#' @export
continue.try_rescue_dose_selector <- function(x, ...) {

  # This selector affects when a trial ends and which dose is selected but only
  # when the parent has selected no dose.
  parent_dose <- x$parent %>% recommended_dose()
  if(is.na(parent_dose)) {
    n_at_dose <- x %>% n_at_dose()
    if(x$dose >= 1 & x$dose <= x %>% num_doses()) {
      if(n_at_dose[x$dose] >= x$n) {
        return(x$parent %>% continue())
      } else {
        return(TRUE)
      }
    }

    # By default:
    return(x$parent %>% continue())
  } else {
    return(x$parent %>% continue())
  }
}
