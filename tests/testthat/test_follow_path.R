test_that('follow_path does what it should.', {

  # Example 1
  model1 <- follow_path(path = '1NNN 2NNN 3NNN 4NNN')

  fit0 <- model1 %>% fit('')
  expect_equal(recommended_dose(fit0), 1)
  expect_equal(continue(fit0), TRUE)

  fit1 <- model1 %>% fit('1NNN 2N')
  expect_equal(recommended_dose(fit1), 2)
  expect_equal(continue(fit1), TRUE)

  fit2 <- model1 %>% fit('1NNN 2NT')
  expect_equal(recommended_dose(fit2), NA)
  expect_equal(continue(fit2), FALSE)


  # Example 2
  model2 <- follow_path(path = '')
  fit3 <- model2 %>% fit('1NNN 2N')
  expect_equal(recommended_dose(fit3), NA)
  expect_equal(continue(fit3), FALSE)

})


test_that('follow_path_selector supports correct interface.', {

  model_fitter <- follow_path(path = '1NNN 2NNN 3NNN 4NNN')

  # Example 1, using outcome string
  x <- fit(model_fitter, '1NNN 2NNN')

  expect_true(is.null(tox_target(x)))

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

  expect_equal(num_doses(x), 4)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:4)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), 3)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0))
  expect_true(is.numeric(prob_administer(x)))

  expect_equal(tox_at_dose(x), c(0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))



  # Example 2, using trivial outcome string
  x <- fit(model_fitter, '')

  expect_true(is.null(tox_target(x)))

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

  expect_equal(num_doses(x), 4)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:4)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), 1)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(0,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_true(is.numeric(prob_administer(x)))

  expect_equal(tox_at_dose(x), c(0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))



  # Example 3, using tibble of outcomes
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0,0, 0,0,1)
  )
  x <- fit(model_fitter, outcomes)

  expect_true(is.null(tox_target(x)))

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

  expect_equal(num_doses(x), 4)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:4)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), NA)

  expect_equal(continue(x), FALSE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0))
  expect_true(is.numeric(prob_administer(x)))

  expect_equal(tox_at_dose(x), c(0,1,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))




  # Example 4, using trivial path string
  model_fitter <- follow_path(path = '')
  x <- fit(model_fitter, '1NNN 2NNT')

  expect_true(is.null(tox_target(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))

  expect_equal(tox(x), c(0,0,0, 0,0,1))
  expect_true(is.integer(tox(x)))

  expect_true(all(model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                              cohort = c(1,1,1,2,2,2),
                                              dose = c(1,1,1,2,2,2),
                                              tox = c(0,0,0,0,0,1)) == 0))

  expect_equal(num_doses(x), 2)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:2)
  expect_true(is.integer(dose_indices(x)))

  expect_equal(recommended_dose(x), NA)

  expect_equal(continue(x), FALSE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(tox_at_dose(x), c(0,1))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.logical(supports_sampling(x)))

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

})


test_that('follow_path_selector interacts appropriately with dfcrm', {

  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25

  model1 <- follow_path('1NN 2NN 3NN') %>%
    get_dfcrm(skeleton = skeleton, target = target)


  fit <- model1 %>% fit('1NN 2N')
  expect_equal(recommended_dose(fit), 2)
  expect_equal(continue(fit), TRUE)
  expect_equal(tox_target(fit), 0.25)


  fit <- model1 %>% fit('1NN 2T')
  expect_equal(recommended_dose(fit), fit$dfcrm_fit$mtd)
  expect_equal(continue(fit), TRUE)


  fit <- model1 %>% fit('1NN 2NN 3NN')
  expect_equal(recommended_dose(fit), fit$dfcrm_fit$mtd)
  expect_equal(continue(fit), TRUE)
})
