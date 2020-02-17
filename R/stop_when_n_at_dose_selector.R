
#' Stop when there are n patients at a dose.
#'
#' This method stops a dose-finding trial when there are n patients at a dose.
#' It can stop when the rule is triggered at the recommended dose, at a
#' particular dose, or at any dose.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param n Stop when there are n at a dose.
#' @param dose \code{'any'} to stop when there are n at any dose;
#' \code{'recommended'} to stop when there are n at the recommended dose; or an
#' integer to stop when there are n at a particular dose-level.
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
#'# This model will stop when 12 are seen at any dose:
#' model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_n_at_dose(n = 12, dose = 'any')
#'
#' # This model fit will not stop:
#' model1 %>% fit('1NNN 2NTN 2TNN 2NNN') %>% continue()
#' # But this model fit will stop:
#' model1 %>% fit('1NNN 2NTN 2TNN 2NNN 2NTT') %>% continue()
#'
#' # This model will stop when 12 are seen at the recommended dose:
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_n_at_dose(n = 12, dose = 'recommended')
#'
#' # This model fit will not stop:
#' fit2 <- model2 %>% fit('1NNN 2NTN 2TNN 2NNN')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#' # But this model fit will stop:
#' fit3 <- model2 %>% fit('1NNN 2NTN 2TNN 2NNN 2NNT')
#' fit3 %>% recommended_dose()
#' fit3 %>% continue()
stop_when_n_at_dose <- function(parent_selector_factory, n, dose) {

  x <- list(
    parent = parent_selector_factory,
    n = n,
    dose = dose
  )
  class(x) <- c('stop_when_n_at_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

stop_when_n_at_dose_selector <- function(parent_selector, n, dose) {

  l <- list(
    parent = parent_selector,
    n = n,
    dose = dose
  )

  class(l) = c('stop_when_n_at_dose_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.stop_when_n_at_dose_selector_factory <- function(selector_factory,
                                                     outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(stop_when_n_at_dose_selector(parent_selector = parent_selector,
                                      n = selector_factory$n,
                                      dose = selector_factory$dose))
}

# Selector interface

#' @importFrom magrittr %>%
#' @export
continue.stop_when_n_at_dose_selector <- function(selector, ...) {
  n_at_dose <- selector %>% n_at_dose()
  if(selector$dose == 'any') {
    if(any(n_at_dose >= selector$n)) {
      return(FALSE)
    }
  }
  else if(selector$dose == 'recommended') {
    rec_dose <- selector %>% recommended_dose()
    if(n_at_dose[rec_dose] >= selector$n) {
      return(FALSE)
    }
  }
  else if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
    if(n_at_dose[selector$dose] >= selector$n) {
      return(FALSE)
    }
  }

  # By default:
  return(selector$parent %>% continue())
}
