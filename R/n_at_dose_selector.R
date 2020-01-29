
stop_when_n_at_dose <- function(parent_selector_factory, n, dose) {

  x <- list(
    parent = parent_selector_factory,
    n = n,
    dose = dose
  )
  class(x) <- c('selector_factory', 'n_at_dose_selector_factory')
  return(x)
}

n_at_dose_selector <- function(parent_selector, n, dose) {

  l <- list(
    parent = parent_selector,
    n = n,
    dose = dose
  )

  class(l) = c('selector', 'derived_dose_selector', 'n_at_dose_selector')
  l
}

# Factory interface
fit.n_at_dose_selector_factory <- function(selector_factory, outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(n_at_dose_selector(parent_selector = parent_selector,
                            n = selector_factory$n,
                            dose = selector_factory$dose))
}

# Selector interface
continue.n_at_dose_selector <- function(selector, ...) {
  # n_at_dose <- selector$parent %>% n_at_dose()
  n_at_dose <- selector %>% n_at_dose()
  if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
    if(n_at_dose[selector$dose] >= selector$n) {
      return(FALSE)
    }
  }

  # By default:
  return(TRUE)
}
