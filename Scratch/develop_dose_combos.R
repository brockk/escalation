
library(escalation)
library(tidyverse)
library(testthat)

# My data in a two tmt trial ----
num_doses <- c(3, 4)

# outcomes <- "2N 3NN 3T"
# outcomes <- "2.1N 3.1NN 3.2T"
# outcomes <- "2.1N 3.1EE 3.2T"
# outcomes <- "2.1N 3.1TTT 3.2N"
outcomes <- "2.1N 2.2TTT 3.2N"
parse_phase1_outcomes(outcomes, as_list = TRUE)
parse_phase1_outcomes(outcomes, as_list = FALSE)

model <- get_boin_comb(num_doses = num_doses, target = 0.25,
                       use_stopping_rule = TRUE)
# model <- get_boin_comb(num_doses = num_doses, target = 0.25,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_n_at_dose(n = 4, dose = c(2, 2)) # Fails on continue

x <- model %>% fit(outcomes = outcomes)
recommended_dose(x) # OK
continue(x) # OK
tox_target(x) # OK
num_patients(x) # OK
cohort(x) # OK
doses_given(x) # OK (it is a list)
tox(x) # OK
num_tox(x) # OK
model_frame(x) # OK
num_doses(x) # OK
dose_indices(x) # OK (a list)
dose_strings(x) # OK
n_at_dose(x) # OK
n_at_recommended_dose(x) # OK
is_randomising(x) # OK
prob_administer(x) # OK
tox_at_dose(x) # OK
empiric_tox_rate(x) # OK
supports_sampling(x) # OK
eff(x) # OK
num_eff(x) # OK
eff_at_dose(x) # OK
empiric_eff_rate(x) # OK
prob_eff_samples(x) # OK
mean_prob_eff(x) # OK
median_prob_eff(x) # OK
prob_eff_quantile(x) # OK
prob_eff_exceeds(x) # OK
utility(x) # OK
weight(x) # OK
mean_prob_tox(x) # OK
median_prob_tox(x) # OK
median_prob_tox(x, iso = FALSE) %>% round(4) # OK
prob_tox_quantile(x, p = 0.9) # OK
prob_tox_exceeds(x, threshold = target) # OK
prob_tox_exceeds(x, threshold = target, iso = FALSE) # OK
dose_admissible(x) # OK
as_tibble(x) # OK
summary(x) # OK
print(x) # OK
# Permissably absent
expect_error(prob_tox_samples(x))
expect_error(tox_limit(x))
expect_error(eff_limit(x))


# My data in a three tmt trial ----
num_doses <- c(2, 3, 2)

df <-
  bind_rows(
    tibble(
      patient = 1,
      cohort = 1,
      dose = list(c(2, 1, 1)),
      tox = 0,
      eff = 0
    ),
    tibble(
      patient = 2,
      cohort = 2,
      dose = list(c(2, 2, 1)),
      tox = 0,
      eff = 0
    ),
    tibble(
      patient = 3,
      cohort = 3,
      dose = list(c(2, 2, 2)),
      tox = 1,
      eff = 1
    ),
    tibble(
      patient = 4,
      cohort = 3,
      dose = list(c(1, 1, 2)),
      tox = 0,
      eff = 1
    )
  )

model <- get_boin_comb(num_doses = num_doses, target = 0.25,
                       use_stopping_rule = NULL)
x <- model %>% fit(outcomes = df) # incorrect number of dimensions

# Removed ----
outcomes <-
  bind_rows(
    tibble(
      patient = 1,
      cohort = 1,
      dose = list(c(2, 1)),
      tox = 0,
      eff = 0
    ),
    tibble(
      patient = 2,
      cohort = 2,
      dose = list(c(3, 1)),
      tox = 0,
      eff = 1
    ),
    tibble(
      patient = 3,
      cohort = 2,
      dose = list(c(3, 1)),
      tox = 0,
      eff = 1
    ),
    tibble(
      patient = 4,
      cohort = 3,
      dose = list(c(3, 2)),
      tox = 1,
      eff = 0
    )
  )
