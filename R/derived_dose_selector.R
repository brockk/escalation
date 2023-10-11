
# Selector interface

#' @export
tox_target.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% tox_target(...))
}

#' @export
tox_limit.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% tox_limit(...))
}

#' @export
eff_limit.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% eff_limit(...))
}

#' @export
num_patients.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% num_patients(...))
}

#' @export
cohort.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% cohort(...))
}

#' @export
doses_given.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% doses_given(...))
}

#' @export
tox.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% tox(...))
}

#' @export
eff.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% eff(...))
}

#' @export
num_doses.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% num_doses(...))
}

#' @export
recommended_dose.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% recommended_dose(...))
}

#' @export
continue.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% continue(...))
}

#' @export
n_at_dose.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% n_at_dose(...))
}

#' @export
#' @importFrom tibble tibble
model_frame.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% model_frame(...))
}

#' @export
is_randomising.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% is_randomising(...))
}

#' @export
tox_at_dose.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% tox_at_dose(...))
}

#' @export
eff_at_dose.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% eff_at_dose(...))
}

#' @export
empiric_tox_rate.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% empiric_tox_rate(...))
}

#' @export
mean_prob_tox.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% mean_prob_tox(...))
}

#' @export
median_prob_tox.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% median_prob_tox(...))
}

#' @export
empiric_eff_rate.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% empiric_eff_rate(...))
}

#' @export
mean_prob_eff.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% mean_prob_eff(...))
}

#' @export
median_prob_eff.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% median_prob_eff(...))
}

#' @export
dose_admissible.derived_dose_selector <- function(x, ...) {
  return(x$parent %>% dose_admissible(...))
}

#' @export
prob_tox_quantile.derived_dose_selector <- function(x, p, ...) {
  return(x$parent %>% prob_tox_quantile(p, ...))
}

#' @export
prob_tox_exceeds.derived_dose_selector <- function(x, threshold, ...) {
  return(x$parent %>% prob_tox_exceeds(threshold, ...))
}

#' @export
prob_eff_quantile.derived_dose_selector <- function(x, p, ...) {
  return(x$parent %>% prob_eff_quantile(p, ...))
}

#' @export
prob_eff_exceeds.derived_dose_selector <- function(x, threshold, ...) {
  return(x$parent %>% prob_eff_exceeds(threshold, ...))
}

#' @export
supports_sampling.derived_dose_selector <- function(x, ...) {
  return(supports_sampling(x$parent))
}

#' @export
prob_tox_samples.derived_dose_selector <- function(x, tall = FALSE, ...) {
  return(prob_tox_samples(x$parent, tall = tall, ...))
}

#' @export
prob_eff_samples.derived_dose_selector <- function(x, tall = FALSE, ...) {
  return(prob_eff_samples(x$parent, tall = tall, ...))
}
