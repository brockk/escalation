
source('R/interface.R')
source('R/phase1_outcomes_to_cohorts.R')
source('R/parse_phase1_outcomes.R')
source('R/dfcrm_selector.R')
source('R/derived_dose_selector.R')
source('R/selector.R')
source('R/n_at_dose_selector.R')
source('R/stop_at_n_selector.R')
source('R/stop_when_too_toxic.R')

library(magrittr)

# Parsing ----
outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
phase1_outcomes_to_cohorts(outcomes)
parse_phase1_outcomes(outcomes)
parse_phase1_outcomes(outcomes, as_list = FALSE)


# Model fitting ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

## Classic R
crm_fitter <- get_dfcrm(skeleton, target)
# Factory interface
x <- fit(crm_fitter, outcomes)
# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, 0.5)


## tidyverse R
crm_fit <- get_dfcrm(skeleton, target) %>%
  fit(outcomes)
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 6, dose = 2) %>%
  fit(outcomes)
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 15) %>%
  fit(outcomes)
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  fit(outcomes)
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.5, confidence = 0.7) %>%
  fit(outcomes)
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  stop_when_too_toxic(dose = 5, tox_threshold = 0.5, confidence = 0.7) %>%
  fit(outcomes)
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_too_toxic(dose = 5, tox_threshold = 0.5, confidence = 0.7) %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  fit(outcomes)

crm_fit %>% class()
crm_fit %>% num_patients()
crm_fit %>% cohort()
crm_fit %>% doses_given()
crm_fit %>% tox()
crm_fit %>% model_frame()
crm_fit %>% num_doses()
crm_fit %>% recommended_dose()
crm_fit %>% continue()
crm_fit %>% n_at_dose()
crm_fit %>% tox_at_dose()
crm_fit %>% empiric_tox_rate()
crm_fit %>% mean_prob_tox()
crm_fit %>% median_prob_tox()
crm_fit %>% prob_tox_exceeds(0.5)



# Tests ----

# of stop_when_n_at_dose
get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 3, dose = 2) %>%
  fit('1NNN 2NTN') %>%
  continue()
get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 3, dose = -1) %>%
  fit('1NNN 2NTN') %>%
  continue()
get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 3, dose = 0) %>%
  fit('1NNN 2NTN') %>%
  continue()
get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 3, dose = 6) %>%
  fit('1NNN 2NTN') %>%
  continue()
get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 4, dose = 2) %>%
  fit('1NNN 2NTN') %>%
  continue()


# Order of embellishments should not matter:
crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 9, dose = 3) %>%
  stop_at_n(n = 21) %>%
  fit(outcomes)
crm_fit %>% continue()
crm_fit %>% recommended_dose()

crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 9, dose = 3) %>%
  fit(outcomes)
crm_fit %>% continue()
crm_fit %>% recommended_dose()


# of stop_at_n
