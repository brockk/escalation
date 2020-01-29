
stop_at_n <- function(parent_selector_factory, n) {

  x <- list(
    parent = parent_selector_factory,
    n = n
  )
  class(x) <- c('selector_factory', 'stop_at_n_selector_factory')
  return(x)
}

stop_at_n_selector <- function(parent_selector, n) {

  l <- list(
    parent = parent_selector,
    n = n
  )

  class(l) = c('selector', 'derived_dose_selector', 'stop_at_n_selector')
  l
}

# Factory interface
fit.stop_at_n_selector_factory <- function(selector_factory, outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(stop_at_n_selector(parent_selector = parent_selector,
                            n = selector_factory$n))
}

# Selector interface
continue.stop_at_n_selector <- function(selector, ...) {
  return(selector %>% num_patients() < selector$n)

  # if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
  #   if(n_at_dose[selector$dose] >= selector$n) {
  #     return(FALSE)
  #   }
  # }
  #
  # # By default:
  # return(TRUE)
}
