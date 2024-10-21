
library(escalation)

{
  library(tidyverse)
  library(testthat)

  # My data in a two tmt trial ----
  num_doses <- c(3, 4)
  df <-
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

  "1.2NNN 2.2NTN"
df
  model <- get_boin_comb(num_doses = num_doses, target = 0.25,
                         use_stopping_rule = NULL)
  x <- model %>% fit(outcomes = df)
  # class(x)
  # names(x)
  # model_frame(x)
  # summary(x)
  # print(x)
  # as_tibble(x)
  # # x # Error
  # boin_fitter <- get_boin(num_doses = 5, target = 0.3)
  # x2 <- fit(boin_fitter, '1NNN')
  # model_frame(x2)
  # summary(x2)
  # print(x2)
  # as_tibble(x2)

  tox_target(x) # OK
  num_patients(x) # OK
  cohort(x) # OK
  doses_given(x) # OK (it is a list)
  tox(x) # OK
  num_tox(x) # OK
  model_frame(x) # OK
  num_doses(x) # OK
  dose_indices(x) # OK
  dose_strings(x) # OK
  recommended_dose(x) # OK
  continue(x) # OK
  n_at_dose(x) # OK
  n_at_recommended_dose(x) # OK
  is_randomising(x) # OK
  prob_administer(x) # OK
  tox_at_dose(x) # OK
  empiric_tox_rate(x) # OK
  supports_sampling(x) # OK
  expect_error(prob_tox_samples(x)) # OK
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
}

# Permissably absent
tox_limit(x)
eff_limit(x)

# Still to do
dose_admissible(x) # Missing
mean_prob_tox(x) # Missing, select.mtd.comb at end of trial.
median_prob_tox(x) # Missing
prob_tox_quantile(x) # Missing
prob_tox_exceeds(x) # Missing

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
