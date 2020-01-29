
# Selector interface
num_patients.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% num_patients(...))
}

cohort.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% cohort(...))
}

doses_given.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% doses_given(...))
}

tox.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% tox(...))
}

num_doses.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% num_doses(...))
}

recommended_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% recommended_dose(...))
}

n_at_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% n_at_dose(...))
}

tox_at_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% tox_at_dose(...))
}

empiric_tox_rate.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% empiric_tox_rate(...))
}

mean_prob_tox.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% mean_prob_tox(...))
}

median_prob_tox.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% median_prob_tox(...))
}

prob_tox_exceeds.derived_dose_selector <- function(selector, threshold, ...) {
  return(selector$parent %>% prob_tox_exceeds(threshold, ...))
}
