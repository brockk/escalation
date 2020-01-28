
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

recommended_dose <- function(selector, ...) {
  UseMethod('recommended_dose')
}

recommended_dose.dfcrm_selector <- function(selector, ...) {
  selector$dfcrm_fit$mtd
}

