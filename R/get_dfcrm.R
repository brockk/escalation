
get_dfcrm <- function(skeleton, target) {

  x <- list(
    skeleton = skeleton,
    target = target
  )
  class(x) <- c('selector_factory', 'dfcrm_selector_factory')
  return(x)
}

fit <- function(selector_factory, outcomes, ...) {
  UseMethod('fit')
}

fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {
  return(dfcrm_selector(outcomes,
                        selector_factory$skeleton,
                        selector_factory$target))
}
