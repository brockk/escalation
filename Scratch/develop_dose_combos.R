
library(escalation)
library(tidyverse)
library(testthat)

# My data in a two tmt trial ----
num_doses <- c(3, 4)

if(FALSE) {
  # outcomes <- "2N 3NN 3T"
  # outcomes <- "2.1N 3.1NN 3.2T"
  # outcomes <- "2.1N 3.1EE 3.2T"
  outcomes <- "2.1N 3.1TTT 3.2N"
  parse_phase1_outcomes(outcomes, as_list = TRUE)
  parse_phase1_outcomes(outcomes, as_list = FALSE)
}
# outcomes <- "2.1N 2.2TTT 3.2N"
outcomes <- "2.1N 2.2TTT 3.2NNNN"

target <- 0.25
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE)
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_n_at_dose(n = 4, dose = c(2, 2))
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_n_at_dose(n = 3, dose = c(2, 2))
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_n_at_dose(n = 4, dose = "recommended")
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_at_n(n = 9)
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_at_n(n = 8)
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   demand_n_at_dose(n = 6, dose = c(2, 2))
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_too_toxic(dose = c(1, 1), tox_threshold = target, confidence = 0.7)
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_too_toxic(dose = "recommended", tox_threshold = target,
#                       confidence = 0.8)
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   # stop_when_tox_ci_covered(dose = 'recommended', lower = 0.15, upper = 0.35)
#   stop_when_tox_ci_covered(dose = 'recommended', lower = 0.01, upper = 0.99)
# model <- get_boin_comb(num_doses = num_doses, target = target,
#                        use_stopping_rule = TRUE) %>%
#   stop_when_too_toxic(dose = "any", tox_threshold = target,
#                       confidence = 0.01) %>%
#   try_rescue_dose(n = 3, dose = c(1, 1))

x <- model %>% fit(outcomes = outcomes)
class(x)

num_doses(x) # OK
dose_indices(x) # OK (a list)
dose_strings(x) # OK
tox_target(x) # OK
is_randomising(x) # OK
supports_sampling(x) # OK

num_patients(x) # OK
doses_given(x) # OK
cohort(x) # OK
tox(x) # OK
weight(x) # OK
num_tox(x) # OK
n_at_dose(x) # OK
model_frame(x) # OK
n_at_dose(x, dose = "recommended") # OK
n_at_dose(x, dose = c(2, 2)) # OK
n_at_recommended_dose(x) # OK
prob_administer(x) # OK

recommended_dose(x) # OK
continue(x) # OK
dose_admissible(x) # OK

tox_at_dose(x) # OK
mean_prob_tox(x) # OK
median_prob_tox(x) # OK
median_prob_tox(x, iso = FALSE) %>% round(4) # OK
prob_tox_quantile(x, p = 0.9) # OK
prob_tox_exceeds(x, threshold = target) # OK
prob_tox_exceeds(x, threshold = target, iso = FALSE) # OK
empiric_tox_rate(x) # OK


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

as_tibble(x) # OK
summary(x) # OK
print(x) # OK
# Permissably absent
expect_error(prob_tox_samples(x))
expect_error(tox_limit(x))
expect_error(eff_limit(x))

1
# My data in a three tmt trial ----
# num_doses <- c(2, 3, 2)
#
# df <-
#   bind_rows(
#     tibble(
#       patient = 1,
#       cohort = 1,
#       dose = list(c(2, 1, 1)),
#       tox = 0,
#       eff = 0
#     ),
#     tibble(
#       patient = 2,
#       cohort = 2,
#       dose = list(c(2, 2, 1)),
#       tox = 0,
#       eff = 0
#     ),
#     tibble(
#       patient = 3,
#       cohort = 3,
#       dose = list(c(2, 2, 2)),
#       tox = 1,
#       eff = 1
#     ),
#     tibble(
#       patient = 4,
#       cohort = 3,
#       dose = list(c(1, 1, 2)),
#       tox = 0,
#       eff = 1
#     )
#   )
#
# model <- get_boin_comb(num_doses = num_doses, target = 0.25,
#                        use_stopping_rule = NULL)
# x <- model %>% fit(outcomes = df) # incorrect number of dimensions

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
