
library(escalation)
library(testthat)
library(dfcrm)
library(dplyr)

selector_factory = get_dfcrm_tite(
  skeleton = getprior(halfwidth = 0.1, target = 0.25, nu = 2, nlevel = 5),
  target = 0.25
) %>%
  stop_at_n(n = 10)
# simulation_function(selector_factory)
true_prob_tox = c(0.04, 0.12, 0.22, 0.35, 0.5)

# # Scratch
# max_time = 2
# previous_outcomes = ""
# time_now = NULL
# i_like_big_trials = FALSE
# sample_patient_arrivals = function(df) cohorts_of_n(n=1, mean_time_delta=1)
# get_weight = linear_follow_up_weight
# patient_sample = PatientSample$new()
# library(dplyr)

# Scheme 0
set.seed(2024)
sims0 = simulate_trials(
  selector_factory,
  num_sims = 10,
  true_prob_tox = true_prob_tox,
  max_time = 2
)
expect_equal(
  max(num_patients(sims0)),
  10
)

# Scheme 1 - Make it longer by slowing recruitment
set.seed(2024)
sims1 = simulate_trials(
  selector_factory,
  num_sims = 10,
  true_prob_tox,
  sample_patient_arrivals = function(df) cohorts_of_n(n = 1, mean_time_delta=2),
  max_time = 2
)
expect_equal(
  max(num_patients(sims1)),
  10
)
expect_lt(
  mean(trial_duration(sims0)),
  mean(trial_duration(sims1))
)

# Scheme 2 - Make it longer still by lengthening evaluation window
set.seed(2024)
sims2 = simulate_trials(
  selector_factory,
  num_sims = 10,
  true_prob_tox,
  sample_patient_arrivals = function(df) cohorts_of_n(n = 1, mean_time_delta=2),
  max_time = 20
)
expect_equal(
  max(num_patients(sims2)),
  10
)
expect_lt(
  mean(trial_duration(sims1)),
  mean(trial_duration(sims2))
)

# Scheme 3 - Add previous patients and start time at 0
set.seed(2024)
sims3 = simulate_trials(
  selector_factory,
  num_sims = 10,
  true_prob_tox,
  sample_patient_arrivals = function(df) cohorts_of_n(n = 1, mean_time_delta=2),
  previous_outcomes = tibble(
    cohort = c(1, 1, 1),
    dose = c(1, 1, 1),
    time = c(0, 0.2, 0.5),
    tox = c(0, 1, 0)
  ),
  time_now = 0,
  max_time = 20
)
expect_equal(
  max(num_patients(sims3)),
  10
)
expect_lt(
  mean(trial_duration(sims3)),
  mean(trial_duration(sims2))
)

# Scheme 4 - specify both patient-sample and patient-time sampling function.
sample_patient_arrivals = function(df, mean_time_delta = 1) {
  time_delta <- rexp(n = 1, rate = 1 / mean_time_delta)
  if(nrow(df) < 6) {
    time_delta <- max(time_delta, 1)
  }
  data.frame(time_delta = time_delta)
}
sims4 = simulate_trials(
  selector_factory,
  num_sims = 10,
  true_prob_tox,
  patient_sample = CorrelatedPatientSample$new(rho = -0.5),
  sample_patient_arrivals = sample_patient_arrivals,
  max_time = 2
)
expect_equal(
  max(num_patients(sims4)),
  10
)
