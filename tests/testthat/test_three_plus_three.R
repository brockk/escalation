
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
  expect_equal(x$recommended_dose, NA)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1TTT', num_doses = 5, strict_mode = FALSE)
  expect_equal(x$recommended_dose, NA)
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
  expect_equal(x$recommended_dose, NA)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 1TTT', num_doses = 5,
                        strict_mode = FALSE)
  expect_equal(x$recommended_dose, NA)
  expect_equal(x$continue, FALSE)
})
