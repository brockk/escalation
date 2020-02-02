
test_that('dfcrm_dose_selector matches dfcrm', {

  # Example 1 - Empiric model, non-standard scale parameter
  skeleton <- c(0.1, 0.2, 0.4, 0.55)
  target <- 0.2
  scale = sqrt(0.75)
  outcomes <- '2NNT 2NNN 3NTT 2NNT'

  # dosefinding model
  model <- get_dfcrm(skeleton = skeleton, target = target, scale = scale)
  x <- model %>% fit(outcomes)

  # dfcrm model
  y <- dfcrm::crm(prior = skeleton, target = target, scale = scale,
                  tox = c(0,0,1, 0,0,0, 0,1,1, 0,0,1),
                  level = c(2,2,2, 2,2,2, 3,3,3, 2,2,2))

  expect_equal(recommended_dose(x), y$mtd)
  expect_equal(round(mean_prob_tox(x), 2),  round(y$ptox, 2))
  expect_equal(x$dfcrm_fit$model, 'empiric')
  expect_equal(x$dfcrm_fit$prior.var, 0.75)


  # Example 2 - Logit model, non-standard intercept parameter
  skeleton <- c(0.1, 0.2, 0.33, 0.45, 0.6, 0.7, 0.8)
  target <- 0.33
  outcomes <- '1NNN 2NNN 3NTT 2NNN 3TNN 3TNT 2NNN'

  # dosefinding model
  model <- get_dfcrm(skeleton = skeleton, target = target, intcpt = 4,
                     model = 'logistic')
  x <- model %>% fit(outcomes)


  # dfcrm model
  y <- dfcrm::crm(prior = skeleton, target = target, intcpt = 4,
                  model = 'logistic',
                  tox = c(0,0,0, 0,0,0, 0,1,1, 0,0,0, 1,0,0, 1,0,1, 0,0,0),
                  level = c(1,1,1, 2,2,2, 3,3,3, 2,2,2, 3,3,3, 3,3,3, 2,2,2))

  expect_equal(recommended_dose(x), y$mtd)
  expect_equal(round(mean_prob_tox(x), 2),  round(y$ptox, 2))
  expect_equal(x$dfcrm_fit$model, 'logistic')
  expect_equal(x$dfcrm_fit$intcpt, 4)
})

test_that('dfcrm_selector supports correct interface.', {
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25
  crm_fitter <- get_dfcrm(skeleton, target)

  # Using outcome string
  x <- fit(crm_fitter, '1NNN 2NTT')

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))

  expect_equal(tox(x), c(0,0,0, 0,1,1))
  expect_true(is.integer(tox(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1))) == 0))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(tox(x)))

  expect_equal(recommended_dose(x), 1)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.numeric(mean_prob_tox(x)))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))


  # Using tibble
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0, 0,0, 1,1)
  )
  x <- fit(crm_fitter, outcomes)

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))

  expect_equal(doses_given(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(doses_given(x)))

  expect_equal(tox(x), c(0,0,0, 0,1,1))
  expect_true(is.integer(tox(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1))) == 0))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(tox(x)))

  expect_equal(recommended_dose(x), 1)
  expect_true(is.integer(recommended_dose(x)))

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  expect_true(is.numeric(mean_prob_tox(x)))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))

})
