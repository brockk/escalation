
get_dfcrm <- function(skeleton, target) {

  x <- list(
    skeleton = skeleton,
    target = target
  )
  class(x) <- c('selector_factory', 'dfcrm_selector_factory')
  return(x)
}

dfcrm_selector <- function(outcomes, skeleton, target) {

  df <- parse_phase1_outcomes(outcomes)
  x <- dfcrm::crm(prior = skeleton, target = target,
                  tox = df$tox, level = df$dose)

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    skeleton = skeleton,
    dfcrm_fit = x
  )

  class(l) = c('selector', 'dfcrm_selector')
  l
}

# Factory interface
fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {
  return(dfcrm_selector(outcomes,
                        selector_factory$skeleton,
                        selector_factory$target))
}

# Selector interface
num_patients.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level %>% length)
}

cohort.dfcrm_selector <- function(selector, ...) {
  return(selector$cohort)
}

doses_given.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level)
}

tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$tox)
}

num_doses.dfcrm_selector <- function(selector, ...) {
  return(length(selector$skeleton))
}

recommended_dose.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$mtd)
}

continue.dfcrm_selector <- function(selector, ...) {
  return(TRUE)
}

n_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  purrr::map_int(dose_indices, ~ sum(selector %>% doses_given() == .x))
}

tox_at_dose.dfcrm_selector <- function(selector, ...) {
  # df <- selector %>% model_frame()
  # df
  dose_indices <- 1:(selector %>% num_doses())
  tox_seen <- selector %>% tox()
  purrr::map_int(dose_indices,
                 ~ sum(tox_seen[selector %>% doses_given() == .x])
  )
}
