
library(escalation)
library(testthat)

# Table 2
tpi_fitter <- get_tpi(num_doses = 8, target = 0.25, k1 = 1, k2 = 1.5,
                      exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)


sc1 <- c(0.05, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
sc2 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.25, 0.5)
sc3 <- c(0.01, 0.05, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
sc4 <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
sc5 <- c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85)
sc6 <- c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75)


# Scenario 1 ----
set.seed(123)
sims1 <- tpi_fitter %>%
  simulate_trials(num_sims = 1000, true_prob_tox = sc1, next_dose = 1)
expect_equal(
  unname(prob_recommend(sims1)),
  c(0, 0.13, 0.79, 0.08, 0, 0, 0, 0, 0),
  tolerance = 0.1
)
expect_equal(
  unname(colMeans(n_at_dose(sims1))),
  c(7.7, 16.1, 5.8, 0.5, 0, 0, 0, 0),
  tolerance = 0.2
)
expect_equal(
  mean(num_tox(sims1) / num_patients(sims1)),
  0.25,
  tolerance = 0.1
)
expect_equal(
  mean(num_patients(sims1)),
  30,
  tolerance = 0.1
)



# Scenario 2 ----
set.seed(123)
sims2 <- tpi_fitter %>%
  simulate_trials(num_sims = 1000, true_prob_tox = sc2, next_dose = 1)
prob_recommend(sims2)
colMeans(n_at_dose(sims2))
sum(num_tox(sims2)) / sum(num_patients(sims2))
mean(num_patients(sims2))


# Scenario 3 ----
set.seed(123)
sims3 <- tpi_fitter %>%
  simulate_trials(num_sims = 1000, true_prob_tox = sc3, next_dose = 1)
prob_recommend(sims3)
colMeans(n_at_dose(sims3))
sum(num_tox(sims3)) / sum(num_patients(sims3))
mean(num_patients(sims3))


# Scenario 4 ----
set.seed(123)
sims4 <- tpi_fitter %>%
  simulate_trials(num_sims = 1000, true_prob_tox = sc4, next_dose = 1)
prob_recommend(sims4)
colMeans(n_at_dose(sims4))
sum(num_tox(sims4)) / sum(num_patients(sims4))
mean(num_patients(sims4))


# Scenario 5 ----
set.seed(123)
sims5 <- tpi_fitter %>%
  simulate_trials(num_sims = 1000, true_prob_tox = sc5, next_dose = 1)
prob_recommend(sims5)
colMeans(n_at_dose(sims5))
sum(num_tox(sims5)) / sum(num_patients(sims5))
mean(num_patients(sims5))


# Scenario 6 ----
set.seed(123)
sims6 <- tpi_fitter %>%
  simulate_trials(num_sims = 1000, true_prob_tox = sc6, next_dose = 1)
prob_recommend(sims6)
colMeans(n_at_dose(sims6))
sum(num_tox(sims6)) / sum(num_patients(sims6))
mean(num_patients(sims6))

# Reproduced, in my opinion.
