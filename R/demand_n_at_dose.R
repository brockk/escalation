
#' Demand there are n patients at a dose before condisdering stopping.
#'
#' This method continues a dose-finding trial until there are n patients at a
#' dose. Once that condition is met, it delegates stopping responsibility to its
#' parent dose selector, whatever that might be. This class is greedy in that it
#' meets its own needs before asking any other selectors in a chain what they
#' want. Thus, different behaviours may be achieved by nesting dose selectors
#' in different orders. See examples.
#'
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param n Continue at least until there are n at a dose.
#' @param dose \code{'any'} to continue until there are n at any dose;
#' \code{'recommended'} to continue until there are n at the recommended dose;
#' or an integer to continue until there are n at a particular dose-level.
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
#' # This model will demand 9 at any dose before it countenances stopping.
#' model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   demand_n_at_dose(n = 9, dose = 'any')
#'
#' # This model will recommend continuing:
#' model1 %>% fit('1NNT 1NNN 2TNN 2NNN') %>% continue()
#' # It tells you to continue because there is no selector considering when
#' # you should stop - dfcrm implements no stopping rule by default.
#'
#' # In contrast, we can add a stopping selector to discern the behaviour of
#' # demand_n_at_dose. We will demand 9 are seen at the recommended dose before
#' # stopping is permitted in model3:
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12)
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   demand_n_at_dose(n = 9, dose = 'recommended')
#'
#' # This model advocates stopping because 12 patients are seen in total:
#' model2 %>% fit('1NNN 1NNN 2TNN 2NNN') %>% continue()
#' # But this model advocates continuing because 9 patients have not been seen
#' # at any dose yet:
#' model3 %>% fit('1NNN 1NNN 2TNN 2NNN') %>% continue()
#' # This shows how demand_n_at_dose overrides stopping behaviours that come
#' # before it in the daisychain.
#'
#' # Once 9 are seen at the recommended dose, the decision to stop is made:
#' fit <- model3 %>% fit('1NNN 1NNN 2TNN 2NNN 2TTN')
#' fit %>% continue()
#' fit %>% recommended_dose()
demand_n_at_dose <- function(parent_selector_factory, n, dose) {

  x <- list(
    parent = parent_selector_factory,
    n = n,
    dose = dose
  )
  class(x) <- c('demand_n_at_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

demand_n_at_dose_selector <- function(parent_selector, n, dose) {

  l <- list(
    parent = parent_selector,
    n = n,
    dose = dose
  )

  class(l) = c('demand_n_at_dose_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.demand_n_at_dose_selector_factory <- function(selector_factory,
                                                  outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(demand_n_at_dose_selector(parent_selector = parent_selector,
                                   n = selector_factory$n,
                                   dose = selector_factory$dose))
}

# Selector interface

#' @importFrom magrittr %>%
#' @export
continue.demand_n_at_dose_selector <- function(x, ...) {

  # This selector affects when a trial ends but not which dose is selected.
  # If the parent selects no dose, this selector changes nothing.
  parent_dose <- x$parent %>% recommended_dose()
  if(any(is.na(parent_dose))) {
    return(x$parent %>% continue())
  }

  if(is.character(x$dose)) {
    # There are certain textual values of dose with accepted meanings.
    # Parse those:
    if(x$dose == "any") {
      n_at_d <- x %>% n_at_dose()
      if(any(n_at_d >= x$n)) {
        return(x$parent %>% continue())
      } else {
        return(TRUE)
      }
    }
    else if(x$dose == "recommended") {
      rec_dose <- x %>% recommended_dose()
      n_at_rec_d <- n_at_dose(x, dose = rec_dose)
      if(n_at_rec_d >= x$n) {
        return(x$parent %>% continue())
      } else {
        return(TRUE)
      }
    }
  } else {
    n_at_d <- n_at_dose(x$parent, dose = x$dose)
    if(n_at_d >= x$n) {
      return(x$parent %>% continue())
    } else {
      return(TRUE)
    }
  }

  # By default:
  return(x$parent %>% continue())
}
