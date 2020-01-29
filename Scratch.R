
source('R/interface.R')
source('R/phase1_outcomes_to_cohorts.R')
source('R/parse_phase1_outcomes.R')
source('R/dfcrm_selector.R')
source('R/derived_dose_selector.R')
source('R/n_at_dose_selector.R')

library(magrittr)

# Parsing ----
outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
coh <- phase1_outcomes_to_cohorts(outcomes)
coh
df <- parse_phase1_outcomes(outcomes)
df


# Model fitting ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

## Classic R
crm_fitter <- get_dfcrm(skeleton, target)
# Factory interface
x <- fit(crm_fitter, outcomes)
# Selector interface
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)


## tidyverse R
crm_fit <- get_dfcrm(skeleton, target) %>%
  fit(outcomes)
crm_fit %>% class()
crm_fit %>% num_doses()
crm_fit %>% recommended_dose()
crm_fit %>% continue()
crm_fit %>% n_at_dose()

crm_fit <- get_dfcrm(skeleton, target) %>%
  stop_when_n_at_dose(n = 3, dose = 2) %>%
  fit(outcomes)
crm_fit %>% class()
crm_fit %>% num_doses()
crm_fit %>% recommended_dose()
crm_fit %>% continue()
crm_fit %>% n_at_dose()


# Tests
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
