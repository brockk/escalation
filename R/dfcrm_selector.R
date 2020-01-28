
get_dfcrm <- function(skeleton, target) {

  x <- list(
    skeleton = skeleton,
    target = target
  )
  class(x) <- c('selector_factory', 'dfcrm_selector_factory')
  return(x)
}

dfcrm_selector <- function(outcomes, skeleton, target) {

  # Parse outcomes
  x <- dfcrm::crm(prior = skeleton, target = target,
                  tox = c(0,0,0, 0,1,0),
                  level = c(1,1,1, 2,2,2))

  l <- list(
    outcomes = outcomes,
    skeleton = skeleton,
    dfcrm_fit = x
  )

  class(l) = c('selector', 'dfcrm_selector')
  l
}

fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {
  return(dfcrm_selector(outcomes,
                        selector_factory$skeleton,
                        selector_factory$target))
}

recommended_dose.dfcrm_selector <- function(selector, ...) {
  selector$dfcrm_fit$mtd
}

n_at_dose.dfcrm_selector <- function(selector, ...) {
  return(c(3,3,0,0,0)) # TODO
}
