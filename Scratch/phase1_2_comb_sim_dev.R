
library(escalation)

"1NNN 2TNN"

follow_path("1.1N 1.2N 1.3N 2.1N") %>%
  get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 9)

# Dev ----
previous_outcomes <- "1.1NNN"
num_doses <- c(3, 4)
target <- 0.25
# selector_factory <-
#   get_boin_comb(num_doses = num_doses, target = target) %>%
#   stop_at_n(n = 9)
selector_factory <-
  follow_path("1.1N 1.2N 1.3N 2.1N") %>%
  get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 9)

selector_factory %>% fit("1.1N")
# Error

# %>% stop_when_n_at_dose(n = 12, dose = "recommended")
next_dose <- NULL
i_like_big_trials <- FALSE
return_all_fits <- TRUE
sample_patient_arrivals = function(df) cohorts_of_n(n=3, mean_time_delta=1)
true_prob_tox <- matrix(c(0.1, 0.2, 0.3, 0.4,
                          0.25, 0.35, 0.45, 0.55,
                          0.3, 0.4, 0.5, 0.6),
                        nrow = num_doses[1],
                        ncol = num_doses[2],
                        byrow = TRUE
)
true_prob_eff <- NULL
patient_sample = PatientSample$new()

# next_dose <- c(2, 1)
# true_prob_tox[t(cbind(next_dose))]

# Test ----
library(escalation)
sessionInfo(package = "escalation")$otherPkgs %>% .[[1]] %>% .[["Version"]]

num_doses <- c(3, 4)
target <- 0.25
model1 <- get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 9)
x0 <- fit(model1, outcomes = "")
x0
# outcomes = ""
# source("R/helpers.R")
# library(tidyverse)
# source("R/outcomes_to_arrays.R")
# use_stopping_rule = TRUE

x1 <- fit(model1, outcomes = "1.1NNN")
x1

model2 <- get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 12)
fit(model2, outcomes = "1.1NNN 2.1TNT")

true_prob_tox <- matrix(c(0.1, 0.2, 0.3, 0.4,
                          0.25, 0.35, 0.45, 0.55,
                          0.3, 0.4, 0.5, 0.6),
                        nrow = num_doses[1],
                        ncol = num_doses[2],
                        byrow = TRUE)

sims1 <- model1 %>%
  simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)

class(sims1)
length(sims1)

num_patients(sims1) # OK
num_doses(sims1) # OK
dose_indices(sims1) # OK
dose_strings(sims1) # OK
recommended_dose(sims1, dose_string = TRUE) # OK
recommended_dose(sims1, dose_string = FALSE) # OK
n_at_dose(sims1) # OK
n_at_dose(sims1, dose = "recommended") # OK
n_at_recommended_dose(sims1) # OK
tox_at_dose(sims1) # OK
num_tox(sims1) # OK
eff_at_dose(sims1) # OK
num_eff(sims1) # OK
# Why not n*m?
prob_recommend(sims1) # OK
prob_administer(sims1, method = 0) # OK
prob_administer(sims1, method = 1) # OK
trial_duration(sims1) # OK
print(sims1) # OK
summary(sims1) # OK
tibble::as_tibble(sims1) # OK
