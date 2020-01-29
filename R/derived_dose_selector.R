
# Selector interface
num_doses.derived_dose_selector <- function(selector, ...) {
    selector$parent %>% num_doses(...)
}

recommended_dose.derived_dose_selector <- function(selector, ...) {
  selector$parent %>% recommended_dose(...)
}

n_at_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% n_at_dose(...))
}
