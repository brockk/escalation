# Iasonos ----
target <- 0.25

iasonos_summarise_crm_sims <- function(sims) {
}


# Scenario 1 ----
sc1_prob_tox <- c(0.03, 0.05, 0.10, 0.18, 0.22)
sc1_skeleton <- c(0.25, 0.30, 0.40, 0.50, 0.55)

# Fixed sample size
crm_fitter <- get_dfcrm(skeleton = sc1_skeleton, target = target) %>%
  dont_skip_doses(when_escalating = TRUE) %>%
  stop_at_n(n = 20)

ecrm_fitter <- follow_path('1N 2N 3NN 4NN 5NNNNN') %>%
  get_dfcrm(skeleton = sc1_skeleton, target = target) %>%
  stop_at_n(n = 20)

rcrm_fitter <- follow_path('1N 2N 3N 4N 5N') %>%
  get_dfcrm(skeleton = sc1_skeleton, target = target) %>%
  dont_skip_doses(when_escalating = TRUE) %>%
  stop_at_n(n = 20)

gcrm_fitter <- get_dfcrm(skeleton = sc1_skeleton, target = target) %>%
  stop_at_n(n = 20) %>%
  demand_n_at_dose(dose = 'recommended', n = 6)
# This needs work to Implement the CI stopper.

sm_fitter <- NULL

set.seed(123)
system.time(crm_sims_1 <- crm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc1_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = which.min(abs(sc1_skeleton - target))
))  # 9.1s
object.size(crm_sims_1) %>% format(units = 'MB')  # 17MB
num_patients(crm_sims_1)
prob_recommend(crm_sims_1) # 68% at d5
prob_administer(crm_sims_1, method = 0) # 45% at d5
prob_administer(crm_sims_1, method = 1) %>% apply(2, sd) # 30% at d5
num_tox(crm_sims_1) %>% median # 3
num_tox(crm_sims_1) %>% quantile(probs = c(0.25, 0.75)) # 2-4

set.seed(123)
system.time(ecrm_sims_1 <- ecrm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc1_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = 1
))  # 12.9s
object.size(ecrm_sims_1) %>% format(units = 'MB')  # 22MB
num_patients(ecrm_sims_1)
prob_recommend(ecrm_sims_1) # 67% at d5
prob_administer(ecrm_sims_1, method = 0) # 38% at d5
prob_administer(ecrm_sims_1, method = 1) %>% apply(2, sd) # 27% at d5
num_tox(ecrm_sims_1) %>% median # 3
num_tox(ecrm_sims_1) %>% quantile(probs = c(0.25, 0.75)) # 2-4

set.seed(123)
system.time(rcrm_sims_1 <- rcrm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc1_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = 1
))  # 14.4s
object.size(rcrm_sims_1) %>% format(units = 'MB')  # 24MB
num_patients(rcrm_sims_1)
prob_recommend(rcrm_sims_1) # 68% at d5
prob_administer(rcrm_sims_1, method = 0) # 45% at d5
prob_administer(rcrm_sims_1, method = 1) %>% apply(2, sd) # 30% at d5
num_tox(rcrm_sims_1) %>% median # 3
num_tox(rcrm_sims_1) %>% quantile(probs = c(0.25, 0.75)) # 2-4
# Essentially same as CRM


# Scenario 2 ----
sc2_prob_tox <- c(0.06, 0.09, 0.13, 0.16, 0.25)
sc2_skeleton <- c(0.15, 0.20, 0.25, 0.30, 0.40)

# Fixed sample size
crm_fitter <- get_dfcrm(skeleton = sc2_skeleton, target = target) %>%
  dont_skip_doses(when_escalating = TRUE) %>%
  stop_at_n(n = 20)

ecrm_fitter <- follow_path('1N 2N 3NN 4NN 5NNNNN') %>%
  get_dfcrm(skeleton = sc2_skeleton, target = target) %>%
  stop_at_n(n = 20)

rcrm_fitter <- follow_path('1N 2N 3N 4N 5N') %>%
  get_dfcrm(skeleton = sc2_skeleton, target = target) %>%
  dont_skip_doses(when_escalating = TRUE) %>%
  stop_at_n(n = 20)

gcrm_fitter <- get_dfcrm(skeleton = sc2_skeleton, target = target) %>%
  stop_at_n(n = 20) %>%
  demand_n_at_dose(dose = 'recommended', n = 6)
# This needs work to Implement the CI stopper.

sm_fitter <- NULL

set.seed(123)
system.time(crm_sims_2 <- crm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc2_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = which.min(abs(sc1_skeleton - target))
))  # 9.4s
object.size(crm_sims_2) %>% format(units = 'MB')  # 17MB
num_patients(crm_sims_2)
prob_recommend(crm_sims_2) # 65% at d5
prob_administer(crm_sims_2, method = 0) # 43% at d5
prob_administer(crm_sims_2, method = 1) %>% apply(2, sd) # 29% at d5
num_tox(crm_sims_2) %>% median # 3
num_tox(crm_sims_2) %>% quantile(probs = c(0.25, 0.75)) # 3-4.25

set.seed(123)
system.time(ecrm_sims_2 <- ecrm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc2_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = 1
))  # 13.2s
object.size(ecrm_sims_2) %>% format(units = 'MB')  # 22MB
num_patients(ecrm_sims_2)
prob_recommend(ecrm_sims_2) # 63% at d5
prob_administer(ecrm_sims_2, method = 0) # 37% at d5
prob_administer(ecrm_sims_2, method = 1) %>% apply(2, sd) # 26% at d5
num_tox(ecrm_sims_2) %>% median # 3
num_tox(ecrm_sims_2) %>% quantile(probs = c(0.25, 0.75)) # 3-4

set.seed(123)
system.time(rcrm_sims_2 <- rcrm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc2_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = 1
))  # 14.7s
object.size(rcrm_sims_2) %>% format(units = 'MB')  # 24MB
num_patients(rcrm_sims_2)
prob_recommend(rcrm_sims_2) # 65% at d5
prob_administer(rcrm_sims_2, method = 0) # 43% at d5
prob_administer(rcrm_sims_2, method = 1) %>% apply(2, sd) # 29% at d5
num_tox(rcrm_sims_2) %>% median # 3
num_tox(rcrm_sims_2) %>% quantile(probs = c(0.25, 0.75)) # 3-4.25
# Essentially same as CRM


# Scenario 3 ----
sc3_prob_tox <- c(0.06, 0.10, 0.15, 0.19, 0.28)
sc3_skeleton <- c(0.10, 0.15, 0.20, 0.25, 0.35)

# Fixed sample size
crm_fitter <- get_dfcrm(skeleton = sc3_skeleton, target = target) %>%
  dont_skip_doses(when_escalating = TRUE) %>%
  stop_at_n(n = 20)

ecrm_fitter <- follow_path('1N 2N 3NN 4NN 5NNNNN') %>%
  get_dfcrm(skeleton = sc3_skeleton, target = target) %>%
  stop_at_n(n = 20)

rcrm_fitter <- follow_path('1N 2N 3N 4N 5N') %>%
  get_dfcrm(skeleton = sc3_skeleton, target = target) %>%
  dont_skip_doses(when_escalating = TRUE) %>%
  stop_at_n(n = 20)

gcrm_fitter <- get_dfcrm(skeleton = sc3_skeleton, target = target) %>%
  stop_at_n(n = 20) %>%
  demand_n_at_dose(dose = 'recommended', n = 6)
# This needs work to Implement the CI stopper.

sm_fitter <- NULL

set.seed(123)
system.time(crm_sims_3 <- crm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc3_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = which.min(abs(sc1_skeleton - target))
))  # 9.4s
object.size(crm_sims_3) %>% format(units = 'MB')  # 17MB
num_patients(crm_sims_3)
prob_recommend(crm_sims_3) # 57% at d5
prob_administer(crm_sims_3, method = 0) # 40% at d5
prob_administer(crm_sims_3, method = 1) %>% apply(2, sd) # 30% at d5
num_tox(crm_sims_3) %>% median # 4
num_tox(crm_sims_3) %>% quantile(probs = c(0.25, 0.75)) # 3-5

set.seed(123)
system.time(ecrm_sims_3 <- ecrm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc3_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = 1
))  # 13.2s
object.size(ecrm_sims_3) %>% format(units = 'MB')  # 22MB
num_patients(ecrm_sims_3)
prob_recommend(ecrm_sims_3) # 55% at d5
prob_administer(ecrm_sims_3, method = 0) # 33% at d5
prob_administer(ecrm_sims_3, method = 1) %>% apply(2, sd) # 26% at d5
num_tox(ecrm_sims_3) %>% median # 4
num_tox(ecrm_sims_3) %>% quantile(probs = c(0.25, 0.75)) # 3-5

set.seed(123)
system.time(rcrm_sims_3 <- rcrm_fitter %>% simulate_trials(
  num_sims = 100,
  true_prob_tox = sc3_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1,
                                                    mean_time_delta = 1/3),
  next_dose = 1
))  # 14.7s
object.size(rcrm_sims_3) %>% format(units = 'MB')  # 24MB
num_patients(rcrm_sims_3)
prob_recommend(rcrm_sims_3) # 57% at d5
prob_administer(rcrm_sims_3, method = 0) # 40% at d5
prob_administer(rcrm_sims_3, method = 1) %>% apply(2, sd) # 30% at d5
num_tox(rcrm_sims_3) %>% median # 4
num_tox(rcrm_sims_3) %>% quantile(probs = c(0.25, 0.75)) # 3-5
# Essentially same as CRM
