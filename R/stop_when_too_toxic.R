
#' @export
stop_when_too_toxic <- function(parent_selector_factory, dose, tox_threshold,
                                confidence) {

  x <- list(
    parent = parent_selector_factory,
    dose = dose,
    tox_threshold = tox_threshold,
    confidence = confidence
  )
  class(x) <- c('selector_factory', 'stop_when_too_toxic_selector_factory')
  return(x)
}

stop_when_too_toxic_selector <- function(parent_selector, dose, tox_threshold,
                                         confidence) {

  l <- list(
    parent = parent_selector,
    dose = dose,
    tox_threshold = tox_threshold,
    confidence = confidence
  )

  class(l) = c('selector', 'derived_dose_selector',
               'stop_when_too_toxic_selector')
  l
}

# Factory interface

#' @export
fit.stop_when_too_toxic_selector_factory <- function(selector_factory, outcomes,
                                                     ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(stop_when_too_toxic_selector(
    parent_selector = parent_selector,
    dose = selector_factory$dose,
    tox_threshold = selector_factory$tox_threshold,
    confidence = selector_factory$confidence
    )
  )
}

# Selector interface

#' @export
recommended_dose.stop_when_too_toxic_selector <- function(selector, ...) {
  prob_too_tox <- selector %>% prob_tox_exceeds(selector$tox_threshold)
  if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
    if(prob_too_tox[selector$dose] >= selector$confidence) {
      return(NA)
    }
  }

  # By default:
  return(selector$parent %>% recommended_dose())
}

#' @export
continue.stop_when_too_toxic_selector <- function(selector, ...) {
  prob_too_tox <- selector %>% prob_tox_exceeds(selector$tox_threshold)
  if(selector$dose == 'any') {
    if(any(prob_too_tox >= selector$confidence)) {
      return(FALSE)
    }
  }
  else if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
    if(prob_too_tox[selector$dose] >= selector$confidence) {
      return(FALSE)
    }
  }

  # By default:
  return(selector$parent %>% continue())
}
