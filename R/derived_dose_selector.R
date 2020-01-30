
# Selector interface

#' @export
num_patients.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% num_patients(...))
}

#' @export
cohort.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% cohort(...))
}

#' @export
doses_given.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% doses_given(...))
}

#' @export
tox.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% tox(...))
}

#' @export
num_doses.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% num_doses(...))
}

#' @export
recommended_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% recommended_dose(...))
}

#' @export
n_at_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% n_at_dose(...))
}

#' @export
tox_at_dose.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% tox_at_dose(...))
}

#' @export
empiric_tox_rate.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% empiric_tox_rate(...))
}

#' @export
mean_prob_tox.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% mean_prob_tox(...))
}

#' @export
median_prob_tox.derived_dose_selector <- function(selector, ...) {
  return(selector$parent %>% median_prob_tox(...))
}

#' @export
prob_tox_exceeds.derived_dose_selector <- function(selector, threshold, ...) {
  return(selector$parent %>% prob_tox_exceeds(threshold, ...))
}
