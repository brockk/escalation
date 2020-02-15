
library(escalation)
1

# Parsing ----
outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
phase1_outcomes_to_cohorts(outcomes)
parse_phase1_outcomes(outcomes)
parse_phase1_outcomes(outcomes, as_list = FALSE)


# dfcrm ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

## Classic R
crm_fitter <- get_dfcrm(skeleton = skeleton, target = target)
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
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.95)


# Using tibble
outcomes <- tibble::tibble(
  cohort = c(1,1, 2,2, 3,3),
  dose = c(1,1, 2,2, 3,3),
  tox = c(0,0, 0,0, 1,1)
)
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

## logit model
crm_fitter <- get_dfcrm(skeleton = skeleton, target = target, intcpt = 4,
                        model = 'logistic')
x <- crm_fitter %>% fit(outcomes)
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
show_examples <- function(crm_fit) {
  cat('class:', crm_fit %>% class(), '\n')
  cat('num_patients:', crm_fit %>% num_patients(), '\n')
  cat('cohort:', crm_fit %>% cohort(), '\n')
  cat('doses_given:', crm_fit %>% doses_given(), '\n')
  cat('tox:', crm_fit %>% tox(), '\n')
  # print('model_frame:', crm_fit %>% model_frame(), '\n')
  cat('num_doses:', crm_fit %>% num_doses(), '\n')
  cat('recommended_dose:', crm_fit %>% recommended_dose(), '\n')
  cat('continue:', crm_fit %>% continue(), '\n')
  cat('n_at_dose:', crm_fit %>% n_at_dose(), '\n')
  cat('tox_at_dose:', crm_fit %>% tox_at_dose(), '\n')
  cat('empiric_tox_rate:', crm_fit %>% empiric_tox_rate(), '\n')
  cat('mean_prob_tox:', crm_fit %>% mean_prob_tox(), '\n')
  cat('median_prob_tox:', crm_fit %>% median_prob_tox(), '\n')
  cat('prob_tox_exceeds_0.5:', crm_fit %>% prob_tox_exceeds(0.5), '\n')
}

library(magrittr)
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  fit(outcomes = '') %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 6, dose = 2) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 15) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 12, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.5, confidence = 0.7) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  stop_when_too_toxic(dose = 5, tox_threshold = 0.5, confidence = 0.7) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 5, tox_threshold = 0.5, confidence = 0.7) %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(tox_threshold = 0.5, confidence = 0.9, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 12, dose = 'any') %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(tox_threshold = 0.5, confidence = 0.75, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 12, dose = 'any') %>%
  fit(outcomes) %>%
  show_examples()


# BOIN ----
num_doses <- 5
target <- 0.3

boin_fitter <- get_boin(num_doses = num_doses, target = target)

x <- fit(boin_fitter, '1NNN')
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
prob_tox_exceeds(x, target)
x$boin_fit$p_overdose

# Using tibble
outcomes <- data.frame(
  cohort = c(1,1, 2,2, 3,3),
  dose = c(1,1, 2,2, 3,3),
  tox = c(0,0, 0,0, 1,1)
)
x <- fit(boin_fitter, outcomes)
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
prob_tox_exceeds(x, target)
x$boin_fit$p_overdose
prob_tox_exceeds(x, target + 0.1)

# Figure 10 of Yan, Pan, Zhang, Liu & Yuan

num_doses <- 5
target <- 0.3
boin_fitter <- get_boin(num_doses = num_doses, target = target)

x <- fit(boin_fitter, '1NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT 3TNN')
recommended_dose(x)

n_at_dose(x)
tox_at_dose(x)


# 3+3 ----
three_plus_three_fitter <- get_three_plus_three(num_doses = 5)
x <- three_plus_three_fitter %>% fit('1NNN 2NTT')
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


# Using tibble
outcomes <- data.frame(
  cohort = c(1,1,1, 2,2,2),
  dose = c(1,1,1, 2,2,2),
  tox = c(0,0, 0,0, 1,1)
)
three_plus_three(outcomes = outcomes, num_doses = num_doses)
x <- fit(three_plus_three_fitter, outcomes)
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



# Simulation ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

# Sc 1
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
plot(true_prob_tox)

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate(num_sims = 500, true_prob_tox = true_prob_tox) -> threeps
prob_recommend(threeps)

# ThreePlusThree in bcrm
bcrm_3p3 <- bcrm::threep3(
  truep = true_prob_tox,
  threep3.start = 1, threep3.esc.only = TRUE)
bcrm_3p3

summary(num_patients(threeps))
length(threeps)
class(threeps)
recommended_dose(threeps)
n_at_dose(threeps) %>% colMeans()
tox_at_dose(threeps) %>% colMeans() %>% sum
object.size(threeps) %>% format(units = 'MB')

crm_fitter <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12)
crm_fitter %>% simulate(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> crm_sims

prob_recommend(crm_sims)
num_patients(crm_sims)
n_at_dose(crm_sims) %>% colMeans()
n_at_dose(crm_sims) %>% colMeans() %>% sum
tox_at_dose(crm_sims) %>% colMeans()
tox_at_dose(crm_sims) %>% colMeans() %>% sum
# sum((n_at_dose(crm_sims) %>% colMeans()) * true_prob_tox)

boin_fitter <- get_boin(num_doses = length(skeleton), target = target) %>%
  stop_at_n(n = 12)
boin_fitter %>% simulate(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> boin_sims
prob_recommend(boin_sims)
num_patients(boin_sims)
n_at_dose(boin_sims) %>% colMeans()
n_at_dose(boin_sims) %>% colMeans() %>% sum
tox_at_dose(boin_sims) %>% colMeans()
tox_at_dose(boin_sims) %>% colMeans() %>% sum

prob_recommend(threeps)
prob_recommend(crm_sims)
prob_recommend(boin_sims)

# Sc 2
true_prob_tox <- c(0.03, 0.09, 0.16, 0.27, 0.42)

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate(num_sims = 500, true_prob_tox = true_prob_tox) -> threeps
crm_fitter %>% simulate(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> crm_sims
boin_fitter %>% simulate(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> boin_sims

prob_recommend(threeps)
prob_recommend(crm_sims)
prob_recommend(boin_sims)

# ThreePlusThree in bcrm
bcrm_3p3 <- bcrm::threep3(
  truep = true_prob_tox,
  threep3.start = 1, threep3.esc.only = TRUE)
bcrm_3p3




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
system.time(crm_sims_1 <- crm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc1_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(ecrm_sims_1 <- ecrm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc1_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(rcrm_sims_1 <- rcrm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc1_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(crm_sims_2 <- crm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc2_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(ecrm_sims_2 <- ecrm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc2_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(rcrm_sims_2 <- rcrm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc2_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(crm_sims_3 <- crm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc3_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(ecrm_sims_3 <- ecrm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc3_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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
system.time(rcrm_sims_3 <- rcrm_fitter %>% simulate(
  num_sims = 100,
  true_prob_tox = sc3_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 1, mean_time_delta = 1/3),
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


# Stopping-rule approaches




# Scratch ----
get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate(num_sims = 500, true_prob_tox = true_prob_tox) -> threeps
prob_recommend(threeps)
summary(num_patients(threeps))
length(threeps)
class(threeps)
recommended_dose(threeps)
n_at_dose(threeps) %>% colMeans()
tox_at_dose(threeps) %>% colMeans() %>% sum
object.size(threeps) %>% format(units = 'MB')

# ThreePlusThree in bcrm
bcrm_3p3 <- bcrm::threep3(
  truep = true_prob_tox,
  threep3.start = 1, threep3.esc.only = TRUE)
bcrm_3p3


# Help files ----

# Interface
? selector
? selector_factory
? fit
? num_patients
? cohort
? doses_given
? tox
? num_tox
? model_frame
? num_doses
? recommended_dose
? continue
? n_at_dose
? prob_administer
? tox_at_dose
? empiric_tox_rate
? mean_prob_tox
? median_prob_tox
? prob_tox_quantile
? prob_tox_exceeds

# selector_factorys
? get_dfcrm
? get_boin
? get_three_plus_three
? dont_skip_doses
? stop_when_n_at_dose
? stop_at_n
? stop_when_too_toxic
? demand_n_at_dose
? follow_path

