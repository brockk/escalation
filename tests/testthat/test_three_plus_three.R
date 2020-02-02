
test_that('Check 3+3 makes correct recommendations.', {

  x <- three_plus_three(outcomes = '', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1N', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1T', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NN', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NT', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1TT', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNT', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NTT', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, as.integer(NA))
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1TTT', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, as.integer(NA))
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2N', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2T', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2TT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2TTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2N', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2T', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2TT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NNN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NNT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NNN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  # Skip cohorts 3 and 4 to test advice at top dose
  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5N', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNN', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5TTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5N', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NN',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NT',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TT',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NNN',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NNT',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NTT',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT',
                        num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)
})


test_that('Check 3+3 errors when it should.', {

  expect_error(three_plus_three(outcomes = '1NNN 1N', num_doses = 5,
                                strict_mode = TRUE))

  expect_error(three_plus_three(outcomes = '1NNN 1NNN', num_doses = 5,
                                strict_mode = TRUE))

  expect_error(three_plus_three(outcomes = '1NNN 2NNT 2NNN 4N', num_doses = 5,
                                strict_mode = TRUE))

  expect_error(three_plus_three(outcomes = '1NNN 2TTT 1N', num_doses = 5,
                                strict_mode = TRUE))

  # TODO:
  # outcomes = '1NTT 1N' # this will not fail yet
  # outcomes = '1NNN 1T' # this will not fail yet
})


test_that('3+3 advice is sensible even when path has diverged from algorithm', {

  # This section is debatable because path has diverged from the 3+3 algorithm.
  # But the method should try to give sensible advice if strict mode has been
  # turned off.

  x <- three_plus_three(outcomes = '1NNN 1T', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 1NNT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 1NTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, as.integer(NA))
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 1TTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, as.integer(NA))
  expect_equal(x$continue, FALSE)
})


test_that('three_plus_three_selector supports correct interface.', {
  three_plus_three_fitter <- get_three_plus_three(num_doses = 5)

  # Using outcome string
  x <- three_plus_three_fitter %>% fit('1NNN 2NTT')

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

  expect_equal(continue(x), FALSE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

  # Using tibble
  outcomes <- tibble::tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0, 0,0, 1,1)
  )
  x <- fit(three_plus_three_fitter, outcomes)

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

  expect_equal(continue(x), FALSE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))

  expect_true(is.numeric(empiric_tox_rate(x)))

})
