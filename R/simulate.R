
# Cruft ----
# ?
# selector_factory <- crm_fitter
# true_prob_tox = c(0.1, 0.27, 0.38, 0.45, 0.61)
# sample_patient_arrivals = function() cohorts_of_n(n = 3, mean_time_delta = 1)
# previous_outcomes = '1NNN 2NNT'
# next_dose = NULL
# i_like_big_trials = FALSE



# Helpers.R ----
#' @importFrom stats rexp
cohorts_of_n <- function(n = 3, mean_time_delta = 1) {
  time_delta <- rexp(n = n, rate = 1 / mean_time_delta) %>% round(1)
  data.frame(time_delta = time_delta)
}


# New factory interface ----
simulation_function <- function(selector_factory) {
  UseMethod('simulation_function')
}

# New concrete factory interface ----
simulation_function.derived_dose_selector_factory <- function(selector_factory){
  return(selector_factory$parent %>% simulation_function())
}

simulation_function.tox_selector_factory <- function(selector_factory) {
  return(phase1_sim)
}

# New interface for selectors and simulations ----
# num_tox <- function(simulations, ...) {
#   UseMethod('num_tox')
# }
#
# prob_administer <- function(simulations, ...) {
#   UseMethod('prob_administer')
# }

# New interface just for simulations
prob_recommend <- function(simulations, ...) {
  UseMethod('prob_recommend')
}


# New concrete interface for selectors ----
# num_tox.selector <- function(selector, ...) {
#   sum(tox(selector))
# }
#
# prob_administer.selector <- function(selector, ...) {
#   n_doses <- num_doses(selector)
#   n_d <- n_at_dose(selector)
#   names(n_d) <- 1:n_doses
#   n_d / sum(n_d)
# }

# New concrete interface for simulations ----
#' @importFrom purrr map map_int
num_patients.simulations <- function(simulations, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map_int(num_patients)
}


#' @importFrom utils head
num_doses.simulations <- function(simulations, ...) {
  # Have a word with this amount of nesting!
  num_doses(head(simulations, 1)[[1]][[1]])
}

#' @importFrom purrr map map_int
recommended_dose.simulations <- function(simulations, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map_int(recommended_dose)
}

#' @importFrom purrr map
#' @importFrom utils tail
n_at_dose.simulations <- function(simulations, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map(n_at_dose) %>%
    do.call(what = rbind)
}

#' @importFrom purrr map
#' @importFrom utils tail
tox_at_dose.simulations <- function(simulations, ...) {
  x <- simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map(tox_at_dose) %>%
    do.call(what = rbind)
  colnames(x) <- NULL
}

num_tox.simulations <- function(simulations, ...) {
  rowSums(tox_at_dose(simulations, ...))
}

#' @importFrom utils head
#' @importFrom purrr map_int
prob_recommend.simulations <- function(simulations, ...) {
  if(length(simulations) > 0) {
    n_doses <- num_doses(simulations)
    rec_d <- recommended_dose(simulations)
    x <- c(sum(is.na(rec_d)),
           map_int(1:n_doses, ~ sum(rec_d == .x, na.rm = TRUE)))
    names(x) <- c('NoDose', 1:n_doses)
    x / sum(x)
  } else {
    return(NULL)
  }
}

#' @importFrom utils head
#' @importFrom purrr map_int
prob_administer.simulations <- function(simulations, method = 0, ...) {
  if(length(simulations) > 0) {
    if(method == 0) {
      n_doses <- num_doses(simulations)
      total_n_at_dose <- n_at_dose(simulations) %>% colSums()
      names(total_n_at_dose) <- 1:n_doses
      total_n_at_dose / sum(total_n_at_dose)
    } else if(method == 1) {
      simulations %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map(prob_administer) %>%
        do.call(what = rbind)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}


# simulate.R ----
library(purrr)
simulate <- function(selector_factory, num_sims, ...) {
  sim_func <- selector_factory %>% simulation_function()
  l <- lapply(1:num_sims, function(x) sim_func(selector_factory, ...))
  class(l) <- 'simulations'
  l
}

#' @importFrom stats rbinom
phase1_sim <- function(
  selector_factory,
  true_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 3, mean_time_delta = 1),
  previous_outcomes = '',
  next_dose = NULL,
  i_like_big_trials = FALSE # Safety net if stop_trial_func is misspecified...
) {
  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(previous_outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  } else{
    base_df <- parse_phase1_outcomes('', as_list = FALSE)
  }
  dose <- base_df$dose
  tox <- base_df$tox
  cohort <- base_df$cohort
  next_cohort <- ifelse(length(cohort) > 0, max(cohort) + 1, 1)
  if('time' %in% colnames(base_df)) {
    time <- previous_outcomes$time
  } else {
    time <- rep(0, length(dose))
  }

  i <- 1 # loop counter
  max_i <- 30
  time_now <- 0
  fit <- selector_factory %>% fit(base_df)
  if(is.null(next_dose)) next_dose <- fit %>% recommended_dose()
  fits <- list()
  fits[[1]] <- fit
  while(fit %>% continue() & (i_like_big_trials | i < max_i)) {

    new_pts <- sample_patient_arrivals()
    arrival_time_deltas <- cumsum(new_pts$time_delta)
    n_new_pts <- nrow(new_pts)
    new_dose <- rep(next_dose, n_new_pts)
    new_tox <- rbinom(n = n_new_pts, size = 1, prob = true_prob_tox[next_dose])
    new_cohort <- rep(next_cohort, n_new_pts)

    dose <- c(dose, new_dose)
    tox <- c(tox, new_tox)
    cohort <- c(cohort, new_cohort)
    time <- c(time, time_now + arrival_time_deltas)
    new_data = data.frame(
      cohort = cohort,
      patient = 1:length(dose),
      dose = dose,
      tox = tox,
      time = time
    )

    fit <- selector_factory %>% fit(new_data)
    fits[[i + 1]] <- fit
    next_dose <- fit %>% recommended_dose()
    i <- i + 1
    next_cohort <- next_cohort + 1
    time_now <- time_now + max(arrival_time_deltas)
  }

  # Warn about i_like_big_trials if sim stopped because of too big i.

  fits
}
