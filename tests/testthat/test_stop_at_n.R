
test_that('stop_at_n_selector does what it should.', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  # Create CRM model that will stop when 15 patients are evaluated:
  model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 15)

  # With 12 patients, this trial should not stop:
  fit <- model1 %>% fit('1NNN 2NTN 2TNN 2NNN')
  expect_equal(recommended_dose(fit), fit$parent$dfcrm_fit$mtd)
  expect_equal(continue(fit), TRUE)
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  check_dose_selector_consistency(fit)

  # With 15 patients, this trial should stop:
  fit <- model1 %>% fit('1NNN 2NTN 2TNN 2NNN 2NTT')
  expect_equal(recommended_dose(fit), fit$parent$dfcrm_fit$mtd)
  expect_equal(continue(fit), FALSE)
  expect_output(
    print(fit),
    "The model advocates stopping and recommending dose 2."
  )
  check_dose_selector_consistency(fit)

})

test_that('stop_at_n_selector supports correct interface.', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  model_fitter <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 15)

  # Example 1, using outcome string
  x <- fit(model_fitter, '1NNN 2NNN')

  expect_equal(tox_target(x), 0.25)
  expect_true(is.numeric(tox_target(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), c(0,0,0, 0,0,0))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_true(is.numeric(weight(x)))
  expect_equal(length(weight(x)), num_patients(x))

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  expect_true(all(model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                              cohort = c(1,1,1,2,2,2),
                                              dose = c(1,1,1,2,2,2),
                                              tox = c(0,0,0,0,0,0)) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_equal(recommended_dose(x), 5)
  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 3)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_equal(n_at_dose(x, dose = 'recommended'), 0)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 0)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_true(is.data.frame(prob_tox_samples(x)))
  expect_true(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))



  # Example 2, using trivial outcome string
  x <- fit(model_fitter, '')

  expect_equal(tox_target(x), 0.25)
  expect_true(is.numeric(tox_target(x)))

  expect_equal(num_patients(x), 0)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), integer(0))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), integer(0))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), integer(0))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_true(is.numeric(weight(x)))
  expect_equal(length(weight(x)), num_patients(x))

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  mf <- model_frame(x)
  expect_equal(nrow(mf), 0)
  expect_equal(ncol(mf), 4)

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_equal(recommended_dose(x), 1)
  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 0)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_equal(n_at_dose(x, dose = 'recommended'), 0)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 0)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_true(is.data.frame(prob_tox_samples(x)))
  expect_true(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))



  # Example 3, using tibble of outcomes
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0,0, 0,0,1)
  )
  x <- fit(model_fitter, outcomes)

  expect_equal(tox_target(x), 0.25)
  expect_true(is.numeric(tox_target(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), c(0,0,0, 0,0,1))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_true(is.numeric(weight(x)))
  expect_equal(length(weight(x)), num_patients(x))

  expect_equal(num_tox(x), 1)
  expect_true(is.integer(num_tox(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,0,1))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_equal(recommended_dose(x), 2)
  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 3)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_equal(n_at_dose(x, dose = 'recommended'), 3)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 3)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,1,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_true(is.data.frame(prob_tox_samples(x)))
  expect_true(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))

})

test_that('stop_at_n_selector interacts appropriately with other selectors.', {

  # Originating from follow_path
  # In model1, demanding n at recommended dose trumps stopping at 12
  model1 <- follow_path('1NNN 2NNN 2NNN 3NNN 3NNN 3NNN 3NNN') %>%
    stop_at_n(n = 12) %>%
    demand_n_at_dose(dose = 'recommended', n = 9)

  # In model2, stopping at 12 trumps demanding n at recommended dose
  model2 <- follow_path('1NNN 2NNN 2NNN 3NNN 3NNN 3NNN 3NNN') %>%
    demand_n_at_dose(dose = 'recommended', n = 9) %>%
    stop_at_n(n = 12)

  fit1 <- model1 %>% fit('1NNN 2NNN 2NNN 3NNN')
  expect_equal(recommended_dose(fit1), 3)
  expect_equal(continue(fit1), TRUE)
  expect_output(
    print(fit1),
    "The model advocates continuing at dose 3."
  )

  fit2 <- model2 %>% fit('1NNN 2NNN 2NNN 3NNN')
  expect_equal(recommended_dose(fit2), 3)
  expect_equal(continue(fit2), FALSE)
  expect_output(
    print(fit2),
    "The model advocates stopping and recommending dose 3."
  )

  # With more outcomes, both should advocate stopping
  fit3 <- model1 %>% fit('1NNN 2NNN 2NNN 3NNN 3NNN 3NNN')
  expect_equal(recommended_dose(fit3), 3)
  expect_equal(continue(fit3), FALSE)
  expect_output(
    print(fit3),
    "The model advocates stopping and recommending dose 3."
  )

  fit4 <- model2 %>% fit('1NNN 2NNN 2NNN 3NNN 3NNN 3NNN')
  expect_equal(recommended_dose(fit4), 3)
  expect_equal(continue(fit4), FALSE)
  expect_output(
    print(fit4),
    "The model advocates stopping and recommending dose 3."
  )


  # Originating from get_dfcrm
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  # In model3, demanding n at recommended dose trumps stopping at 12
  model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    stop_at_n(n = 12) %>%
    demand_n_at_dose(dose = 'recommended', n = 9)

  # In model4, stopping at 12 trumps demanding n at recommended dose
  model4 <- get_dfcrm(skeleton = skeleton, target = target) %>%
    demand_n_at_dose(dose = 'recommended', n = 9) %>%
    stop_at_n(n = 12)

  fit1 <- model3 %>% fit('1NNN 2NNN 2NNN 3NNN')
  expect_equal(recommended_dose(fit1), 5)
  expect_equal(continue(fit1), TRUE)
  expect_output(
    print(fit1),
    "The model advocates continuing at dose 5."
  )

  fit2 <- model4 %>% fit('1NNN 2NNN 2NNN 3NNN')
  expect_equal(recommended_dose(fit2), 5)
  expect_equal(continue(fit2), FALSE)
  expect_output(
    print(fit2),
    "The model advocates stopping and recommending dose 5."
  )

  # With more outcomes, both should advocate stopping
  fit3 <- model3 %>% fit('1NNN 2NNN 2NNN 5NNN 5NNN 5NNN')
  expect_equal(recommended_dose(fit3), 5)
  expect_equal(continue(fit3), FALSE)
  expect_output(
    print(fit3),
    "The model advocates stopping and recommending dose 5."
  )

  fit4 <- model4 %>% fit('1NNN 2NNN 2NNN 5NNN 5NNN 5NNN')
  expect_equal(recommended_dose(fit4), 5)
  expect_equal(continue(fit4), FALSE)
  expect_output(
    print(fit4),
    "The model advocates stopping and recommending dose 5."
  )

})
