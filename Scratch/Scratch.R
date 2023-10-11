
# Load ----
library(escalation)

target <- 0.25
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)


# Parsing ----
outcomes <- '1NNN 2NEN 3BNT 3NNN 3TNT 2BEE'
phase1_2_outcomes_to_cohorts(outcomes)
parse_phase1_2_outcomes(outcomes)
parse_phase1_2_outcomes(outcomes, as_list = FALSE)


outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
phase1_outcomes_to_cohorts(outcomes)
parse_phase1_outcomes(outcomes)
parse_phase1_outcomes(outcomes, as_list = FALSE)



# dfcrm ----
crm_fitter <- get_dfcrm(skeleton = skeleton, target = target)
# Factory interface
outcomes <- '1NNN 2NNN 3NNT'
x <- fit(crm_fitter, outcomes)
x

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
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_tox_quantile(x, p = 0.95)
# and standard generics
print(x)
summary(x)
dplyr::as_tibble(x)


# trialr crm ----

# Empiric
crm_fitter <- get_trialr_crm(skeleton = skeleton, target = target,
                             model = 'empiric', beta_sd = 1.34)
# 1-param logistic
crm_fitter <- get_trialr_crm(skeleton = skeleton, target = target,
                             model = 'logistic', a0 = 3,
                             beta_mean = 0, beta_sd = 1.34)
# 2-param logistic
crm_fitter <- get_trialr_crm(skeleton = skeleton, target = target,
                             model = 'logistic2',
                             alpha_mean = 0, alpha_sd = 2,
                             beta_mean = 0, beta_sd = 1.34)

# Factory interface
outcomes <- '1NNN 2NNN 3NNT'
x <- fit(crm_fitter, outcomes)
x
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
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_tox_quantile(x, p = 0.95)
# and standard generics
print(x)
summary(x)
dplyr::as_tibble(x)


# TPI example ----
tpi_fitter <- get_tpi(num_doses = 5, target = 0.3, k1 = 1, k2 = 1.5,
                      exclusion_certainty = 0.7)
x <- fit(tpi_fitter, '1NNN')
x <- fit(tpi_fitter, '1NNN 2TTNT')
# x <- fit(tpi_fitter, '1NNN 2TTNT 3NNT')
x

class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)


# mTPI example ----
model <- get_mtpi(num_doses = 5, target = 0.3, epsilon1 = 0.05, epsilon2 = 0.05,
                  exclusion_certainty = 0.95)
x <- fit(model, '1NNN')
x <- fit(model, '1NNN 2TTNT')
x

class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)

# BOIN ----
num_doses <- length(skeleton)
target <- 0.3

boin_fitter <- get_boin(num_doses = num_doses, target = target)
x <- fit(boin_fitter, '1NNN')
x <- fit(boin_fitter, '1NNN 2TTNT 3NNT')
x

class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)
x$boin_fit$p_overdose


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
x

class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
tox_target(x)



# Random selector ----
prob_select = c(0.1, 0.3, 0.5, 0.07, 0.03)
model <- get_random_selector(prob_select = prob_select)
x <- model %>% fit('1NTN 2NN 5TT')
# x <- model %>% fit('1NTN 2EN 5BB') # Fails

# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
eff(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
eff_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
empiric_eff_rate(x)
mean_prob_eff(x)
median_prob_eff(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_eff_exceeds(x, 0.5)
prob_eff_quantile(x, p = 0.05)
prob_eff_quantile(x, p = 0.5)
supports_sampling(x)
# and standard generics
print(x)
summary(x)
dplyr::as_tibble(x)

# trialr EffTox ----
efftox_priors <- trialr::efftox_priors
p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                   beta_mean = 1.5482, beta_sd = 3.5018,
                   gamma_mean = 0.7367, gamma_sd = 2.5423,
                   zeta_mean = 3.4181, zeta_sd = 2.4406,
                   eta_mean = 0, eta_sd = 0.2,
                   psi_mean = 0, psi_sd = 1)
real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)

model <- get_trialr_efftox(real_doses = real_doses,
                           efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                           p_e = 0.1, p_t = 0.1,
                           eff0 = 0.5, tox1 = 0.65,
                           eff_star = 0.7, tox_star = 0.25,
                           priors = p, seed = 2020)
x <- model %>% fit('1N 2E 3B')
x

# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
eff(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
eff_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
empiric_eff_rate(x)
mean_prob_eff(x)
median_prob_eff(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_eff_exceeds(x, 0.5)
prob_eff_quantile(x, p = 0.05)
prob_eff_quantile(x, p = 0.5)
supports_sampling(x)
prob_tox_samples(x)
prob_eff_samples(x)

# and standard generics
print(x)
summary(x)
dplyr::as_tibble(x)

# Wages & Tait ----
tox_skeleton = c(0.01, 0.08, 0.15, 0.22, 0.29, 0.36)
eff_skeletons = matrix(nrow=11, ncol=6)
eff_skeletons[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20, 0.10)
eff_skeletons[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30, 0.20)
eff_skeletons[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40, 0.30)
eff_skeletons[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50, 0.40)
eff_skeletons[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.50)
eff_skeletons[6,] <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60)
eff_skeletons[7,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.60)
eff_skeletons[8,] <- c(0.30, 0.40, 0.50, 0.60, 0.60, 0.60)
eff_skeletons[9,] <- c(0.40, 0.50, 0.60, 0.60, 0.60, 0.60)
eff_skeletons[10,] <- c(0.50, 0.60, 0.60, 0.60, 0.60, 0.60)
eff_skeletons[11,] <- c(rep(0.60, 6))
eff_skeleton_weights = rep(1, nrow(eff_skeletons))
tox_limit = 0.33
eff_limit = 0.05

model <- get_wages_and_tait(tox_skeleton = tox_skeleton,
                            eff_skeletons = eff_skeletons,
                            tox_limit = tox_limit, eff_limit = eff_limit,
                            num_randomise = 20)

x <- model %>% fit('1N 2E 3B')
x

# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
eff(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
eff_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
empiric_eff_rate(x)
mean_prob_eff(x)
median_prob_eff(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_eff_exceeds(x, 0.5)
prob_eff_quantile(x, p = 0.05)
prob_eff_quantile(x, p = 0.5)
supports_sampling(x)
prob_tox_samples(x)
prob_eff_samples(x)

# and standard generics
print(x)
summary(x)
dplyr::as_tibble(x)

# BOIN12 ----
model <- get_boin12(num_doses = 5, phi_t = 0.35, phi_e = 0.25, u2 = 40, u3 = 60)

x <- model %>% fit('1EEB')
x

# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
eff(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
dose_admissible(x)
n_at_dose(x)
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
prob_administer(x)
is_randomising(x)
tox_at_dose(x)
eff_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
empiric_eff_rate(x)
mean_prob_eff(x)
median_prob_eff(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_eff_exceeds(x, 0.5)
prob_eff_quantile(x, p = 0.05)
prob_eff_quantile(x, p = 0.5)
supports_sampling(x)
# prob_tox_samples(x)
# prob_eff_samples(x)

# and standard generics
print(x)
summary(x)
dplyr::as_tibble(x)

# Dose paths ----
cohort_sizes <- c(3, 3, 3)

# Use 3+3
selector_factory <- get_three_plus_three(num_doses = 5)

# Use dfcrm
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
selector_factory <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12)
selector_factory <- get_dfcrm(skeleton = skeleton, target = target) %>%
  dont_skip_doses() %>%
  stop_when_n_at_dose(dose = 'recommended', n = 9)

# Use trialr CRM
# 2-param logistic
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
selector_factory <- get_trialr_crm(skeleton = skeleton, target = target,
                                   model = 'logistic2',
                                   alpha_mean = 0, alpha_sd = 2,
                                   beta_mean = 0, beta_sd = 1.34)


# Use BOIN
selector_factory <- get_boin(num_doses = length(skeleton), target = target) %>%
  stop_at_n(n = 12)

# Use EffTox
efftox_priors <- trialr::efftox_priors
p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                   beta_mean = 1.5482, beta_sd = 3.5018,
                   gamma_mean = 0.7367, gamma_sd = 2.5423,
                   zeta_mean = 3.4181, zeta_sd = 2.4406,
                   eta_mean = 0, eta_sd = 0.2,
                   psi_mean = 0, psi_sd = 1)
real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
selector_factory <- get_trialr_efftox(real_doses = real_doses,
                                      efficacy_hurdle = 0.5,
                                      toxicity_hurdle = 0.3,
                                      p_e = 0.1, p_t = 0.1,
                                      eff0 = 0.5, tox1 = 0.65,
                                      eff_star = 0.7, tox_star = 0.25,
                                      priors = p, seed = 2020)

# Get paths
cohort_sizes <- c(2, 2)
paths1 <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
dplyr::as_tibble(paths1) %>% print(n = 100)

spread_paths(dplyr::as_tibble(paths1)) %>%
  dplyr::select('outcomes0', 'next_dose0', 'outcomes1', 'next_dose1',
                'outcomes2', 'next_dose2') %>%
  print(n=1000)

# In-progress trials:
paths2 <- selector_factory %>%
  get_dose_paths(cohort_sizes = cohort_sizes, previous_outcomes = '1NTN')
dplyr::as_tibble(paths2) %>% print(n = 100)
spread_paths(dplyr::as_tibble(paths2)) %>%
  dplyr::select('outcomes0', 'next_dose0', 'outcomes1', 'next_dose1',
                'outcomes2', 'next_dose2') %>%
  print(n=100)

paths3 <- selector_factory %>%
  get_dose_paths(cohort_sizes = cohort_sizes, previous_outcomes = '1NTT')
dplyr::as_tibble(paths3) %>% print(n = 100)
spread_paths(dplyr::as_tibble(paths3)) %>%
  dplyr::select('outcomes0', 'next_dose0', 'outcomes1', 'next_dose1',
                'outcomes2', 'next_dose2') %>%
  print(n = 100)

paths <- paths1
class(paths) # dose_paths
length(paths) # num_paths
class(paths[[1]]) # dose_finding_path_node

paths
print(paths, node = 10)
print(paths, node = 90)
print(paths, node = -1)
print(paths, node = NA)
print(paths, node = NULL)

# Visualise
graph_paths(paths)
graph_paths(paths, viridis_palette = 'viridis')
graph_paths(paths, viridis_palette = 'magma')
graph_paths(paths, viridis_palette = 'plasma')
graph_paths(paths, viridis_palette = 'inferno')
graph_paths(paths, viridis_palette = 'cividis')
# Or
graph_paths(paths, RColorBrewer_palette = 'YlOrRd')
graph_paths(paths, RColorBrewer_palette = 'Blues')
graph_paths(paths, RColorBrewer_palette = 'Paired')
graph_paths(paths, RColorBrewer_palette = 'RdPu')
graph_paths(paths, RColorBrewer_palette = 'Set2')
graph_paths(paths, RColorBrewer_palette = 'Spectral')
# We could append this graph with transition probabilities.

# Crytallised dose-paths ----
# Using CRM
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
cohort_sizes <- c(3, 3, 5)
paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  get_dose_paths(cohort_sizes = cohort_sizes)
class(paths)
num_doses(paths)
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
x <- calculate_probabilities(paths, true_prob_tox)
x

# Using EffTox
efftox_priors <- trialr::efftox_priors
p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                   beta_mean = 1.5482, beta_sd = 3.5018,
                   gamma_mean = 0.7367, gamma_sd = 2.5423,
                   zeta_mean = 3.4181, zeta_sd = 2.4406,
                   eta_mean = 0, eta_sd = 0.2,
                   psi_mean = 0, psi_sd = 1)
real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
selector_factory <- get_trialr_efftox(real_doses = real_doses,
                                      efficacy_hurdle = 0.5,
                                      toxicity_hurdle = 0.3,
                                      p_e = 0.1, p_t = 0.1,
                                      eff0 = 0.5, tox1 = 0.65,
                                      eff_star = 0.7, tox_star = 0.25,
                                      priors = p, seed = 2020)

cohort_sizes <- c(1, 2)
paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
true_prob_eff <- c(0.27, 0.35, 0.41, 0.44, 0.45)
x <- calculate_probabilities(paths, true_prob_tox, true_prob_eff)
x

# Using random selector
prob_select = c(0.1, 0.3, 0.5, 0.07, 0.03)
selector_factory <- get_random_selector(prob_select = prob_select)
cohort_sizes <- c(1, 2)
paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
x <- calculate_probabilities(paths, true_prob_tox)

selector_factory <- get_random_selector(prob_select = prob_select,
                                        supports_efficacy = TRUE)
paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
true_prob_eff <- c(0.27, 0.35, 0.41, 0.44, 0.45)
x <- calculate_probabilities(paths, true_prob_tox, true_prob_eff)
x


x$supports_efficacy
print(x)
summary(x)
num_patients(x)
num_doses(x)
dose_indices(x)
n_at_dose(x)
sum(n_at_dose(x))
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
sum(tox_at_dose(x))
num_tox(x)
eff_at_dose(x)
sum(eff_at_dose(x))
num_eff(x)
prob_recommend(x)
sum(prob_recommend(x))
prob_administer(x)
sum(prob_administer(x))


# Patient samples ----
set.seed(2023)
patients <- PatientSample$new()
patients$num_patients
patients$tox_u
patients$eff_u
patients$expand_to(num_patients = 2)
patients$num_patients
patients$tox_u
patients$eff_u
patients$get_tox_u(1)
patients$get_tox_u(1)
patients$get_tox_u(2)
patients$get_tox_u(2)
patients$get_patient_tox(i = 1, prob_tox = 0.4)
patients$get_patient_tox(i = 1, prob_tox = 0.5)
patients$get_patient_tox(i = 2, prob_tox = 0.3)
patients$get_patient_tox(i = 2, prob_tox = 0.4)
patients$get_tox_u(i = 3)
patients$get_patient_tox(i = 3, prob_tox = 0.01)
patients$get_patient_tox(i = 3, prob_tox = 0.05)
patients$get_patient_tox(i = 1:3, prob_tox = 0.1)
patients$get_patient_tox(i = 3:5, prob_tox = 0.1)

# Simulation ----

# Sc 1
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)

# Use 3+3
get_three_plus_three(num_doses = 5) %>%
  simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox) -> sims
sims

get_three_plus_three(num_doses = 5) %>%
  simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox,
                  return_all_fits = TRUE) -> sims

# Use dfcrm
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

# Use trialr CRM
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
get_trialr_crm(skeleton = skeleton, target = target,
               model = 'logistic2',
               alpha_mean = 0, alpha_sd = 2,
               beta_mean = 0, beta_sd = 1.34) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

# Use BOIN
set.seed(2020)
get_boin(num_doses = 5, target = target) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

# Use EffTox
efftox_priors <- trialr::efftox_priors
p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                   beta_mean = 1.5482, beta_sd = 3.5018,
                   gamma_mean = 0.7367, gamma_sd = 2.5423,
                   zeta_mean = 3.4181, zeta_sd = 2.4406,
                   eta_mean = 0, eta_sd = 0.2,
                   psi_mean = 0, psi_sd = 1)
real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
model <- get_trialr_efftox(real_doses = real_doses,
                           efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                           p_e = 0.1, p_t = 0.1,
                           eff0 = 0.5, tox1 = 0.65,
                           eff_star = 0.7, tox_star = 0.25,
                           priors = p, seed = 2020) %>%
  stop_at_n(n = 6)

true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
true_prob_eff <- c(0.27, 0.35, 0.41, 0.44, 0.45)

sims <- model %>%
  simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
                  true_prob_eff = true_prob_eff)

# Interface
sims
length(sims)
class(sims)
summary(sims)
summary(num_patients(sims))
num_doses(sims)
dose_indices(sims)
n_at_dose(sims) %>% colMeans()
num_tox(sims) %>% mean
num_eff(sims) %>% mean
tox_at_dose(sims) %>% colMeans()
eff_at_dose(sims) %>% colMeans()
object.size(sims) %>% format(units = 'MB')
recommended_dose(sims)
prob_recommend(sims)
prob_administer(sims)
trial_duration(sims)
summary(trial_duration(sims))
dplyr::as_tibble(sims) %>% print(n = 30)

library(ggplot2)
dplyr::as_tibble(sims) %>%
  ggplot(aes(x = dose, y = mean_prob_tox)) +
  geom_line(aes(group = .iteration))

dplyr::as_tibble(sims) %>%
  ggplot(aes(x = dose, y = mean_prob_tox)) +
  geom_line(aes(group = .iteration)) +
  facet_wrap(~ .iteration)

# ThreePlusThree in bcrm calculates exact OCs.
bcrm_3p3 <- bcrm::threep3(
  truep = true_prob_tox,
  threep3.start = 1, threep3.esc.only = TRUE)
bcrm_3p3



# Sc 2
true_prob_tox <- c(0.03, 0.09, 0.16, 0.27, 0.42)

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate_trials(num_sims = 500, true_prob_tox = true_prob_tox) -> threeps
crm_fitter %>% simulate_trials(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function(current_data)
    cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> crm_sims
boin_fitter %>% simulate_trials(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function(current_data)
    cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> boin_sims

prob_recommend(threeps)
prob_recommend(crm_sims)
prob_recommend(boin_sims)

library(dplyr)
bind_rows(
  tibble(Method = '3+3', Dose = 0:5, ProbSelect = prob_recommend(threeps)),
  tibble(Method = 'CRM', Dose = 0:5, ProbSelect = prob_recommend(crm_sims)),
  tibble(Method = 'BOIN', Dose = 0:5, ProbSelect = prob_recommend(boin_sims))
) %>% ggplot(aes(x = Dose, y = ProbSelect, col = Method)) +
  geom_point() + geom_line() +
  labs(title = 'Model-based vs rule-based dose-finding',
       subtitle = paste0('Model-based methods allocate more patients close to ',
                         'target dose.'))

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
? dose_admissible
? prob_tox_quantile
? prob_tox_exceeds

# selector_factories
? get_dfcrm
? get_boin
? get_tpi
? get_mtpi
? get_three_plus_three
? dont_skip_doses
? stop_when_n_at_dose
? stop_at_n
? stop_when_too_toxic
? demand_n_at_dose
? follow_path
