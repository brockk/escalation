
test_that('stop_when_tox_ci_covered_selector does what it should', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  # Create CRM model that will stop when the CI of recommended dose is within
  # (0.15, 0.35):
  model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_when_tox_ci_covered(dose = 'recommended', lower = 0.15, upper = 0.35)
  # and another that will stop when the CI of recommended dose is within
  # (0.05, 0.45):
  model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_when_tox_ci_covered(dose = 'recommended', lower = 0.05, upper = 0.45)


  # This situation should fire neither stopping rule:
  outcomes <- '1NNN 2NNN 3NNT'
  fit1 <- model1 %>% fit(outcomes)
  fit2 <- model2 %>% fit(outcomes)

  expect_equal(fit1 %>% recommended_dose(), 3)
  expect_true(fit1 %>% continue())
  expect_equal(fit2 %>% recommended_dose(), 3)
  expect_true(fit2 %>% continue())

  # This is because the CI is too wide in each model:
  lower1 <- fit1 %>% prob_tox_quantile(p = 0.05)
  upper1 <- fit1 %>% prob_tox_quantile(p = 0.95)
  expect_false(any(lower1 >= 0.15 & upper1 <= 0.35))
  # But narrow enough in the second model:
  lower2 <- fit2 %>% prob_tox_quantile(p = 0.05)
  upper2 <- fit2 %>% prob_tox_quantile(p = 0.95)
  expect_false(any(lower2 >= 0.05 & upper2 <= 0.55))



  # This situation should not fire the stopping rule in the first but should in
  # the second:
  outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
  fit1 <- model1 %>% fit(outcomes)
  fit2 <- model2 %>% fit(outcomes)

  expect_equal(fit1 %>% recommended_dose(), 3)
  expect_true(fit1 %>% continue())
  expect_equal(fit2 %>% recommended_dose(), 3)
  expect_false(fit2 %>% continue())

  # This is because the CI is too wide in the first model:
  lower1 <- fit1 %>% prob_tox_quantile(p = 0.05)
  upper1 <- fit1 %>% prob_tox_quantile(p = 0.95)
  expect_false(any(lower1 >= 0.15 & upper1 <= 0.35))
  # But narrow enough in the second model:
  lower2 <- fit2 %>% prob_tox_quantile(p = 0.05)
  upper2 <- fit2 %>% prob_tox_quantile(p = 0.95)
  expect_true(any(lower2 >= 0.05 & upper2 <= 0.55))

})

test_that('stop_when_tox_ci_covered_selector supports correct interface.', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  model_fitter <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_when_tox_ci_covered(dose = 'recommended', lower = 0.15, upper = 0.35)

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

  expect_equal(recommended_dose(x), 5)
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
