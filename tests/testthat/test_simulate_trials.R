
test_that('Simulation results are sensible', {

  true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
  num_sims <- 10

  # Scenario 1 - 3+3 simulations
  sims <- get_three_plus_three(num_doses = 5) %>%
    simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox)

  expect_is(sims, "simulations")
  expect_equal(length(sims$fits), num_sims)
  # Expect one model fit each iteration
  expect_true(all(sapply(sims$fit, length) == 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)



  # Scenario 2 - CRM simulations

  # Scenario 2a - plain vanilla
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25
  sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 12) %>%
    simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)
  expect_is(sims, "simulations")
  expect_equal(length(sims$fits), num_sims)
  # Expect one model fit each iteration
  expect_true(all(sapply(sims$fit, length) == 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)


  # Scenario 2b - use previous outcomes
  sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 12) %>%
    simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
                    previous_outcomes = '5TTT')
  expect_is(sims, "simulations")
  expect_equal(length(sims$fits), num_sims)
  # Expect one model fit each iteration
  expect_true(all(sapply(sims$fit, length) == 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)
  # Every simulation should have treated at least 3 patients at dose 5
  expect_true(all(n_at_dose(sims)['5'] >= 3))
  # Every simulation should have seen at least 3 toxes at dose 5
  expect_true(all(tox_at_dose(sims)['5'] >= 3))


  # Scenario 2c - use previous outcomes
  sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 12) %>%
    simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
                    next_dose = 5)
  expect_is(sims, "simulations")
  expect_equal(length(sims$fits), num_sims)
  # Expect one model fit each iteration
  expect_true(all(sapply(sims$fit, length) == 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)
  # Every simulation should have treated at least 3 patients at dose 5
  expect_true(all(n_at_dose(sims)['5'] >= 3))


  # Scenario 2d - breach the limit of 30 model invocations
  # Expect this to warn - the simulation was forcibly ended.
  expect_warning(sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
                   stop_at_n(n = 99) %>%
                   simulate_trials(num_sims = 1, true_prob_tox = true_prob_tox)
  )
  # Nevertheless, it should have succeeded and contain sensible results:
  expect_is(sims, "simulations")
  expect_equal(length(sims$fits), 1)
  # Expect one model fit each iteration
  expect_true(all(sapply(sims$fit, length) == 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)
  # In this scenario, each iteration will have treated 87 patients:
  expect_true(all(num_patients(sims) == 87))


  # Scenario 2e - intentionally breach the limit of 30 model invocations
  # In contrast to 2d, expect this not to warn.
  sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 99) %>%
    simulate_trials(num_sims = 1, true_prob_tox = true_prob_tox,
                    i_like_big_trials = TRUE)
  # Expect one model fit each iteration
  expect_true(all(sapply(sims$fit, length) == 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)
  # Expect no warning
  expect_true(all(num_patients(sims) == 99))


  # Scenario 2f - return all model fits
  sims <- get_three_plus_three(num_doses = 5) %>%
    simulate_trials(num_sims = num_sims, true_prob_tox = true_prob_tox,
                    return_all_fits = TRUE)
  # Expect many model fits per iteration
  expect_true(all(sapply(sims$fit, length) >= 1))
  expect_true(any(sapply(sims$fit, length) > 1))
  # Probability of recommendation should be 1
  expect_true(abs(sum(prob_recommend(sims)) - 1) < 0.01)

})

test_that('3+3 simulations match independent source', {

  # Wheeler et al. published a Shiny app to calculate the exact operating
  # characteristics of 3+3 designs at  https://github.com/graham-wheeler/AplusB.
  # Reproduce using our Monte Carlo simulation method their exact probabilities..

  # Five dose scenario where de-escalation is possible:
  true_prob_tox <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  set.seed(2023)

  # For high precision but slow computation:
  # sims <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE) %>%
  #   simulate_trials(num_sims = 10^4, true_prob_tox = true_prob_tox)
  # expect_equal(
  #   unname(prob_recommend(sims)),
  #   c(0.1, 0.28, 0.33, 0.2, 0.06, 0.02),
  #   tolerance = 0.01
  # )

  # For low precision but quick computation:
  sims <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE) %>%
    simulate_trials(num_sims = 10^2, true_prob_tox = true_prob_tox)
  expect_equal(
    unname(prob_recommend(sims)),
    c(0.1, 0.28, 0.33, 0.2, 0.06, 0.02), # Wheeler stats
    tolerance = 0.1
  )

  # Eight dose scenario where de-escalation is not possible:
  true_prob_tox <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  set.seed(2023)
  sims <- get_three_plus_three(num_doses = 8, allow_deescalate = FALSE) %>%
    simulate_trials(num_sims = 10^2, true_prob_tox = true_prob_tox)
  expect_equal(
    unname(prob_recommend(sims)),
    c(0.09, 0.26, 0.33, 0.22, 0.08, 0.02, 0, 0, 0), # Wheeler stats
    tolerance = 0.1
  )

})

test_that('TPI simulations match independent source', {

  # In Table 2 of:
  # Ji, Y.; Li, Y.; Nebiyou Bekele, B. (2007). Dose-finding in phase I clinical
  # trials based on toxicity probability intervals. , 4(3), 235–244.
  # doi:10.1177/1740774507079442
  # the TPI authors present performance from a simulation study. Replicate some
  # of those statistics here.

  # Design:
  tpi_fitter <- get_tpi(num_doses = 8, target = 0.25, k1 = 1, k2 = 1.5,
                        exclusion_certainty = 0.95) %>%
    stop_at_n(n = 30)

  # Scenario 1 ----
  sc1 <- c(0.05, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
  set.seed(123)
  sims1 <- tpi_fitter %>%
    simulate_trials(num_sims = 10^2, true_prob_tox = sc1, next_dose = 1)
  expect_equal(
    unname(prob_recommend(sims1)),
    c(0, 0.13, 0.79, 0.08, 0, 0, 0, 0, 0),
    tolerance = 0.1
  )
  expect_equal(
    unname(colMeans(n_at_dose(sims1))),
    c(7.7, 16.1, 5.8, 0.5, 0, 0, 0, 0),
    tolerance = 0.3
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

  # If these checks run withoput irritating CRAN, we could add more scenarios.
  # See Scratch/ of GitHub repo.
})

test_that('mTPI simulations match independent source', {

  # In Table 1 of:
  # Yuan Ji, ; Ping Liu, ; Yisheng Li, ; Nebiyou Bekele, B.  (2010). A modified
  # toxicity probability interval method for dose-finding trials. Clinical
  # Trials, 7(6), 653–663. doi:10.1177/1740774510382799
  # the mTPI authors present performance from a simulation study. Replicate some
  # of those statistics here.

  # Design:
  model <- get_mtpi(num_doses = 8, target = 0.25,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95) %>%
    stop_at_n(n = 30)

  # Scenario 1 ----
  sc1 <- c(0.05, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
  set.seed(123)
  sims1 <- model %>%
    simulate_trials(num_sims = 10^2, true_prob_tox = sc1, next_dose = 1)
  expect_equal(
    unname(prob_recommend(sims1)),
    c(0, 0.14, 0.78, 0.08, 0, 0, 0, 0, 0),
    tolerance = 0.1
  )
  expect_equal(
    unname(colMeans(n_at_dose(sims1))),
    c(7.1, 18.3, 4.4, 0.2, 0, 0, 0, 0),
    tolerance = 0.3
  )
  expect_equal(
    mean(num_tox(sims1) / num_patients(sims1)),
    c(0.24),
    tolerance = 0.1
  )
  expect_equal(
    mean(num_patients(sims1)),
    c(30),
    tolerance = 0.1
  )

  # If these checks run withoput irritating CRAN, we could add more scenarios.
  # See Scratch/ of GitHub repo.
})
