
test_that('stick_on_num_tox_selector does what it should.', {

  # Normal mTPI2 behaviour:
  target <- 0.25
  model1 <- get_mtpi2(num_doses = 5, target = target, epsilon1 = 0.05,
                      epsilon2 = 0.05, exclusion_certainty = 0.95)
  x1 <- model1 %>% fit('1NNN 2NTN')
  expect_equal(recommended_dose(x1), 1)
  expect_true(continue(x1))
  expect_output(
    print(x1),
    "The model advocates continuing at dose 1."
  )

  # Contrast to:
  model2 <- get_mtpi2(num_doses = 5, target = target, epsilon1 = 0.05,
                      epsilon2 = 0.05, exclusion_certainty = 0.95) %>%
    stick_on_num_tox(num_tox = 1, num_patients = 3)
  x2 <- model2 %>% fit('1NNN 2NTN')
  expect_equal(recommended_dose(x2), 2)
  expect_true(continue(x2))
  expect_output(
    print(x2),
    "The model advocates continuing at dose 2."
  )
  check_dose_selector_consistency(x2)

  # Test at dose 1
  x <- model2 %>% fit('1NNT')
  expect_equal(recommended_dose(x), 1)
  expect_true(continue(x))
  expect_output(
    print(x),
    "The model advocates continuing at dose 1."
  )
  check_dose_selector_consistency(x)

  # Test at dose 3
  x <- model2 %>% fit('1NTT 2TNT 3TNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))
  expect_output(
    print(x),
    "The model advocates continuing at dose 3."
  )
  check_dose_selector_consistency(x)

  # Test the advise falls away when target conditions not met
  x <- model2 %>% fit('1NTT 2TNT 3TTN')
  expect_equal(recommended_dose(x), 2)
  expect_true(continue(x))
  expect_output(
    print(x),
    "The model advocates continuing at dose 2."
  )
  check_dose_selector_consistency(x)

  x <- model2 %>% fit('1NTT 2TNT 3TTNN')
  expect_equal(recommended_dose(x), 2)
  expect_true(continue(x))
  expect_output(
    print(x),
    "The model advocates continuing at dose 2."
  )
  check_dose_selector_consistency(x)

})


test_that('stop_when_n_at_dose_selector supports correct interface.', {

  target <- 0.25

  model_fitter <- get_mtpi2(num_doses = 5, target = target, epsilon1 = 0.05,
                            epsilon2 = 0.05, exclusion_certainty = 0.95) %>%
    stick_on_num_tox(num_tox = 1, num_patients = 3)


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
                                              tox = c(0,0,0,0,0,0),
                                              weight = c(1,1,1,1,1,1)) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_equal(recommended_dose(x), 3)
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

  expect_error(is.data.frame(prob_tox_samples(x)))
  expect_error(is.data.frame(prob_tox_samples(x, tall = TRUE)))

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
  expect_equal(ncol(mf), 5)

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

  expect_error(is.data.frame(prob_tox_samples(x)))
  expect_error(is.data.frame(prob_tox_samples(x, tall = TRUE)))

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
                                               tox = c(0,0,0,0,0,1),
                                               weight = c(1,1,1,1,1,1))) == 0))
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

  expect_error(is.data.frame(prob_tox_samples(x)))
  expect_error(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))

})
