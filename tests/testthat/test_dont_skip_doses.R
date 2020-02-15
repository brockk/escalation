
test_that('dont_skip_selector does what it should.', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  # Escalation CRM example

  ## Just CRM
  model0 <- get_dfcrm(skeleton = skeleton, target = target)
  fit0 <- model0 %>% fit('1NNN')

  ## CRM Skipping not allowed
  model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    dont_skip_doses()
  fit1 <- model1 %>% fit('1NNN')

  expect_equal(recommended_dose(fit1), 2)
  expect_equal(continue(fit1), TRUE)
  expect_true(recommended_dose(fit0) >= recommended_dose(fit1))

  ## Skipping forcibly allowed, effectively replicating model0
  model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    dont_skip_doses(when_escalating = FALSE, when_deescalating = FALSE)
  fit2 <- model2 %>% fit('1NNN')
  expect_equal(recommended_dose(fit0), recommended_dose(fit2))
  expect_equal(continue(fit0), continue(fit2))


  # De-escalation CRM example

  ## Just CRM
  fit3 <- model0 %>% fit('1NNN 2N 3TTT')

  ## CRM Skipping not allowed
  model4 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    dont_skip_doses(when_deescalating = TRUE)
  fit4 <- model4 %>% fit('1NNN 2N 3TTT')

  expect_equal(recommended_dose(fit4), 2)
  expect_equal(continue(fit4), TRUE)
  expect_true(recommended_dose(fit3) <= recommended_dose(fit4))

  ## Skipping forcibly allowed, effectively replicating fit3
  model5 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    dont_skip_doses(when_escalating = FALSE, when_deescalating = FALSE)
  fit5 <- model2 %>% fit('1NNN 2N 3TTT')
  expect_equal(recommended_dose(fit3), recommended_dose(fit5))
  expect_equal(continue(fit3), continue(fit5))
})


test_that('dont_skip_selector supports correct interface.', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  model_fitter <- get_dfcrm(skeleton = skeleton, target = target) %>%
    dont_skip_doses(when_escalating = TRUE, when_deescalating = FALSE)

  # Example 1, using outcome string
  x <- fit(model_fitter, '1NNN 2NNN')

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))

  expect_equal(tox(x), c(0,0,0, 0,0,0))
  expect_true(is.integer(tox(x)))

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  expect_true(all(model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                              cohort = c(1,1,1,2,2,2),
                                              dose = c(1,1,1,2,2,2),
                                              tox = c(0,0,0,0,0,0)) == 0))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), 3)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
  expect_true(is.numeric(prob_administer(x)))

  expect_equal(tox_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.numeric(mean_prob_tox(x)))

  expect_true(is.numeric(median_prob_tox(x)))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))



  # Example 2, using trivial outcome string
  x <- fit(model_fitter, '')

  expect_equal(num_patients(x), 0)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), integer(0))
  expect_true(is.integer(cohort(x)))

  expect_equal(doses_given(x), integer(0))
  expect_true(is.integer(doses_given(x)))

  expect_equal(tox(x), integer(0))
  expect_true(is.integer(tox(x)))

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  mf <- model_frame(x)
  expect_equal(nrow(mf), 0)
  expect_equal(ncol(mf), 4)

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), 1)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_true(is.numeric(prob_administer(x)))

  expect_equal(tox_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.numeric(mean_prob_tox(x)))

  expect_true(is.numeric(median_prob_tox(x)))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))



  # Example 3, using tibble of outcomes
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0,0, 0,0,1)
  )
  x <- fit(model_fitter, outcomes)

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))

  expect_equal(tox(x), c(0,0,0, 0,0,1))
  expect_true(is.integer(tox(x)))

  expect_equal(num_tox(x), 1)
  expect_true(is.integer(num_tox(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,0,1))) == 0))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), 2)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
  expect_true(is.numeric(prob_administer(x)))

  expect_equal(tox_at_dose(x), c(0,1,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.numeric(mean_prob_tox(x)))

  expect_true(is.numeric(median_prob_tox(x)))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
})
