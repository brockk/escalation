test_that('BOIN recommendations match published example.', {

  num_doses <- 5
  target <- 0.3
  boin_fitter <- get_boin(num_doses = num_doses, target = target)

  x <- fit(boin_fitter, '1NNN')
  expect_equal(recommended_dose(x), 2)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT')
  expect_equal(recommended_dose(x), 2)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN')
  expect_equal(recommended_dose(x), 4)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT 3TNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  expect_equal(n_at_dose(x), c(3, 6, 18, 3, 0))
  expect_equal(tox_at_dose(x), c(0, 1, 5, 3, 0))
})


test_that('boin_selector supports correct interface.', {

  num_doses <- 5
  target <- 0.3

  boin_fitter <- get_boin(num_doses = num_doses, target = target)

  # Using outcome string
  x <- fit(boin_fitter, '1NNN 2NTT')

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


  # Using tibble
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0, 0,0, 1,1)
  )
  x <- fit(boin_fitter, outcomes)

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

})
