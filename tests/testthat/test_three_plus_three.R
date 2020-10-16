
# Tests of three_plus_three
test_that('Check 3+3 makes correct recommendations without de-escalation.', {

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


test_that('Check 3+3 makes correct recommendations with de-escalation.', {

  x <- three_plus_three(outcomes = '', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1N', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1T', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NN', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NT', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1TT', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNT', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NTT', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, as.integer(NA))
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1TTT', num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, as.integer(NA))
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2N', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2T', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2TT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NTT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2TTT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2N', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2T', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2TT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NNN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NNT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NTT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNT 2NNN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  # Skip cohorts 3 and 4 to test advice at top dose
  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5N', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNN', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NTT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5TTT', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5N', num_doses = 5,
                        strict_mode = FALSE, allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NN',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NNN',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 5)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NNT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NTT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4N',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4T',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NN',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4TT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NNN',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NNT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 4)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4TTT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3N',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3T',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3NN',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3NT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3NNN',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3NNT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 3)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3NTT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT',
                        num_doses = 5, strict_mode = FALSE,
                        allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2N',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2T',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NN',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2TT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NNN',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NNT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2TTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1N',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1T',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1NN',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1NT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1TT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1NNN',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1NNT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 1)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_true(is.na(x$recommended_dose))
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1TTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_true(is.na(x$recommended_dose))
  expect_equal(x$continue, FALSE)

  expect_error(x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT 4NTT 3TTT 2NTT 1NTT 0N',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE))


  # A few more manual checks
  x <- three_plus_three(
    outcomes = '1NNN 2NNN 3NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NNN 2NNN 2NNN 3NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NTN 1NNN 2NNN 3NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, TRUE)

  x <- three_plus_three(
    outcomes = '1NTN 1NNN 2NTN 2NNN 3NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NTN 1NTN 2NTN 2NNN 3NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_equal(x$recommended_dose, 2)
  expect_equal(x$continue, FALSE)

  x <- three_plus_three(
    outcomes = '1NTN 1NTN 2NTN 2NTN 3NTT',
    num_doses = 5, strict_mode = FALSE,
    allow_deescalate = TRUE)
  expect_true(is.na(x$recommended_dose))
  expect_equal(x$continue, FALSE)

})


test_that('Check 3+3 errors when it should in strict mode.', {

  expect_error(three_plus_three(outcomes = '1NNN 1N', num_doses = 5,
                                strict_mode = TRUE))

  expect_error(three_plus_three(outcomes = '1NNN 1NNN', num_doses = 5,
                                strict_mode = TRUE))

  expect_error(three_plus_three(outcomes = '1NNN 2NNT 2NNN 4N', num_doses = 5,
                                strict_mode = TRUE))

  expect_error(three_plus_three(outcomes = '1NNN 2TTT 1N', num_doses = 5,
                                strict_mode = TRUE))

  # TODO:
  # outcomes = '1NTT 1N' # does not fail
  # outcomes = '1NNN 1T' # does not fail
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



# Tests of three_plus_three_selector
test_that('three_plus_three_selector does what it should without de-esc', {

  threep_model <- get_three_plus_three(num_doses = 5, allow_deescalate = FALSE)
  expect_false(threep_model$allow_deescalate)

  fit1 <- threep_model %>% fit('1NNN 2NTT')
  expect_false(fit1$allow_deescalate)
  expect_equal(fit1 %>% recommended_dose(), 1)
  expect_false(fit1 %>% continue())

  # I could wholesale copy and paste down from above to further test this class

})


test_that('three_plus_three_selector does what it should with de-esc', {

  threep_model <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE)
  expect_true(threep_model$allow_deescalate)

  fit1 <- threep_model %>% fit('1NNN 2NTT')
  expect_true(fit1$allow_deescalate)
  expect_equal(fit1 %>% recommended_dose(), 1)
  expect_true(fit1 %>% continue())

  # I could wholesale copy and paste down from above to further test this class

})


test_that('three_plus_three_selector de-escalates from doses higher than 1', {

  threep_model <- get_three_plus_three(num_doses = 5, allow_deescalate = TRUE)

  fit <- threep_model %>% fit('2NNT 2NNN')
  expect_equal(fit %>% recommended_dose(), 3)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1N')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1T')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1NN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1NT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1TT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1NNN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1NNT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1NTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NNT 1TTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1N')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1T')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1NN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1NT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1TT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1NNN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1NNT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1NTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2NTT 1TTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1N')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1T')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1NN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1NT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1TT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1NNN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1NNT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1NTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NNT 2TTT 1TTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1N')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1T')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1TT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1TTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1N')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1T')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1NN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1NT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1TT')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_true(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1NNN')
  expect_equal(fit %>% recommended_dose(), 1)
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1NNT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1NTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

  fit <- threep_model %>% fit('2NTT 1NNT 1TTT')
  expect_true(is.na(fit %>% recommended_dose()))
  expect_false(fit %>% continue())

})

test_that(
  'three_plus_three_selector supports correct interface without de-esc.', {

    three_plus_three_fitter <- get_three_plus_three(num_doses = 5,
                                                    allow_deescalate = FALSE)

    # Example 1, using outcome string
    x <- three_plus_three_fitter %>% fit('1NNN 2NTT')

    expect_true(is.null(tox_target(x)))

    expect_equal(num_patients(x), 6)
    expect_true(is.integer(num_patients(x)))

    expect_equal(cohort(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(cohort(x)))
    expect_equal(length(cohort(x)), num_patients(x))

    expect_equal(doses_given(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(doses_given(x)))
    expect_equal(length(doses_given(x)), num_patients(x))

    expect_equal(tox(x), c(0,0,0, 0,1,1))
    expect_true(is.integer(tox(x)))
    expect_equal(length(tox(x)), num_patients(x))

    expect_equal(num_tox(x), 2)
    expect_true(is.integer(num_tox(x)))

    expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                                 cohort = c(1,1,1,2,2,2),
                                                 dose = c(1,1,1,2,2,2),
                                                 tox = c(0,0,0,0,1,1))) == 0))
    expect_equal(nrow(model_frame(x)), num_patients(x))

    expect_equal(num_doses(x), 5)
    expect_true(is.integer(tox(x)))

    expect_equal(dose_indices(x), 1:5)
    expect_true(is.integer(dose_indices(x)))
    expect_equal(length(dose_indices(x)), num_doses(x))

    expect_equal(recommended_dose(x), 1)
    expect_true(is.integer(recommended_dose(x)))
    expect_equal(length(recommended_dose(x)), 1)

    expect_equal(continue(x), FALSE)
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

    expect_equal(tox_at_dose(x), c(0,2,0,0,0))
    expect_true(is.integer(tox_at_dose(x)))
    expect_equal(length(tox_at_dose(x)), num_doses(x))

    expect_true(is.numeric(empiric_tox_rate(x)))
    expect_equal(length(empiric_tox_rate(x)), num_doses(x))

    expect_true(is.logical(supports_sampling(x)))

    expect_error(prob_tox_samples(x))
    expect_error(prob_tox_samples(x, tall = TRUE))

    # Expect summary to not error. This is how that is tested, apparently:
    expect_error(summary(x), NA)
    expect_output(print(x))
    expect_true(tibble::is_tibble(as_tibble(x)))
    expect_true(nrow(as_tibble(x)) >= num_doses(x))


    # Example 2, empty outcome string..
    x <- three_plus_three_fitter %>% fit('')

    expect_true(is.null(tox_target(x)))

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

    expect_true(is.logical(supports_sampling(x)))

    expect_error(prob_tox_samples(x))
    expect_error(prob_tox_samples(x, tall = TRUE))

    # Expect summary to not error. This is how that is tested, apparently:
    expect_error(summary(x), NA)
    expect_output(print(x))
    expect_true(tibble::is_tibble(as_tibble(x)))
    expect_true(nrow(as_tibble(x)) >= num_doses(x))


    # Example 3, using tibble
    outcomes <- tibble::tibble(
      cohort = c(1,1,1, 2,2,2),
      dose = c(1,1,1, 2,2,2),
      tox = c(0,0, 0,0, 1,1)
    )
    x <- fit(three_plus_three_fitter, outcomes)

    expect_true(is.null(tox_target(x)))

    expect_equal(num_patients(x), 6)
    expect_true(is.integer(num_patients(x)))

    expect_equal(cohort(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(cohort(x)))
    expect_equal(length(cohort(x)), num_patients(x))

    expect_equal(doses_given(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(doses_given(x)))
    expect_equal(length(doses_given(x)), num_patients(x))

    expect_equal(tox(x), c(0,0,0, 0,1,1))
    expect_true(is.integer(tox(x)))
    expect_equal(length(tox(x)), num_patients(x))

    expect_equal(num_tox(x), 2)
    expect_true(is.integer(num_tox(x)))

    expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                                 cohort = c(1,1,1,2,2,2),
                                                 dose = c(1,1,1,2,2,2),
                                                 tox = c(0,0,0,0,1,1))) == 0))
    expect_equal(nrow(model_frame(x)), num_patients(x))

    expect_equal(num_doses(x), 5)
    expect_true(is.integer(tox(x)))

    expect_equal(dose_indices(x), 1:5)
    expect_true(is.integer(dose_indices(x)))
    expect_equal(length(dose_indices(x)), num_doses(x))

    expect_equal(recommended_dose(x), 1)
    expect_true(is.integer(recommended_dose(x)))
    expect_equal(length(recommended_dose(x)), 1)

    expect_equal(continue(x), FALSE)
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

    expect_equal(tox_at_dose(x), c(0,2,0,0,0))
    expect_true(is.integer(tox_at_dose(x)))
    expect_equal(length(tox_at_dose(x)), num_doses(x))

    expect_true(is.numeric(empiric_tox_rate(x)))
    expect_equal(length(empiric_tox_rate(x)), num_doses(x))

    expect_true(is.logical(supports_sampling(x)))

    expect_error(prob_tox_samples(x))
    expect_error(prob_tox_samples(x, tall = TRUE))

    # Expect summary to not error. This is how that is tested, apparently:
    expect_error(summary(x), NA)
    expect_output(print(x))
    expect_true(tibble::is_tibble(as_tibble(x)))
    expect_true(nrow(as_tibble(x)) >= num_doses(x))

  })


test_that(
  'three_plus_three_selector supports correct interface with de-esc.', {

    three_plus_three_fitter <- get_three_plus_three(num_doses = 5,
                                                    allow_deescalate = TRUE)

    # Example 1, using outcome string
    x <- three_plus_three_fitter %>% fit('1NNN 2NTT')

    expect_true(is.null(tox_target(x)))

    expect_equal(num_patients(x), 6)
    expect_true(is.integer(num_patients(x)))

    expect_equal(cohort(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(cohort(x)))
    expect_equal(length(cohort(x)), num_patients(x))

    expect_equal(doses_given(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(doses_given(x)))
    expect_equal(length(doses_given(x)), num_patients(x))

    expect_equal(tox(x), c(0,0,0, 0,1,1))
    expect_true(is.integer(tox(x)))
    expect_equal(length(tox(x)), num_patients(x))

    expect_equal(num_tox(x), 2)
    expect_true(is.integer(num_tox(x)))

    expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                                 cohort = c(1,1,1,2,2,2),
                                                 dose = c(1,1,1,2,2,2),
                                                 tox = c(0,0,0,0,1,1))) == 0))
    expect_equal(nrow(model_frame(x)), num_patients(x))

    expect_equal(num_doses(x), 5)
    expect_true(is.integer(tox(x)))

    expect_equal(dose_indices(x), 1:5)
    expect_true(is.integer(dose_indices(x)))
    expect_equal(length(dose_indices(x)), num_doses(x))

    expect_equal(recommended_dose(x), 1)
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

    expect_equal(tox_at_dose(x), c(0,2,0,0,0))
    expect_true(is.integer(tox_at_dose(x)))
    expect_equal(length(tox_at_dose(x)), num_doses(x))

    expect_true(is.numeric(empiric_tox_rate(x)))
    expect_equal(length(empiric_tox_rate(x)), num_doses(x))

    expect_true(is.logical(supports_sampling(x)))

    expect_error(prob_tox_samples(x))
    expect_error(prob_tox_samples(x, tall = TRUE))

    # Expect summary to not error. This is how that is tested, apparently:
    expect_error(summary(x), NA)
    expect_output(print(x))
    expect_true(tibble::is_tibble(as_tibble(x)))
    expect_true(nrow(as_tibble(x)) >= num_doses(x))


    # Example 2, empty outcome string..
    x <- three_plus_three_fitter %>% fit('')

    expect_true(is.null(tox_target(x)))

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

    expect_true(is.logical(supports_sampling(x)))

    expect_error(prob_tox_samples(x))
    expect_error(prob_tox_samples(x, tall = TRUE))

    # Expect summary to not error. This is how that is tested, apparently:
    expect_error(summary(x), NA)
    expect_output(print(x))
    expect_true(tibble::is_tibble(as_tibble(x)))
    expect_true(nrow(as_tibble(x)) >= num_doses(x))


    # Example 3, using tibble
    outcomes <- tibble::tibble(
      cohort = c(1,1,1, 2,2,2),
      dose = c(1,1,1, 2,2,2),
      tox = c(0,0, 0,0, 1,1)
    )
    x <- fit(three_plus_three_fitter, outcomes)

    expect_true(is.null(tox_target(x)))

    expect_equal(num_patients(x), 6)
    expect_true(is.integer(num_patients(x)))

    expect_equal(cohort(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(cohort(x)))
    expect_equal(length(cohort(x)), num_patients(x))

    expect_equal(doses_given(x), c(1,1,1, 2,2,2))
    expect_true(is.integer(doses_given(x)))
    expect_equal(length(doses_given(x)), num_patients(x))

    expect_equal(tox(x), c(0,0,0, 0,1,1))
    expect_true(is.integer(tox(x)))
    expect_equal(length(tox(x)), num_patients(x))

    expect_equal(num_tox(x), 2)
    expect_true(is.integer(num_tox(x)))

    expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                                 cohort = c(1,1,1,2,2,2),
                                                 dose = c(1,1,1,2,2,2),
                                                 tox = c(0,0,0,0,1,1))) == 0))
    expect_equal(nrow(model_frame(x)), num_patients(x))

    expect_equal(num_doses(x), 5)
    expect_true(is.integer(tox(x)))

    expect_equal(dose_indices(x), 1:5)
    expect_true(is.integer(dose_indices(x)))
    expect_equal(length(dose_indices(x)), num_doses(x))

    expect_equal(recommended_dose(x), 1)
    expect_true(is.integer(recommended_dose(x)))
    expect_equal(length(recommended_dose(x)), 1)

    expect_equal(continue(x), TRUE)
    expect_true(is.logical(continue(x)))

    expect_equal(n_at_dose(x), c(3,3,0,0,0))
    expect_true(is.integer(n_at_dose(x)))
    expect_equal(length(n_at_dose(x)), 5)

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

    expect_equal(tox_at_dose(x), c(0,2,0,0,0))
    expect_true(is.integer(tox_at_dose(x)))
    expect_equal(length(tox_at_dose(x)), num_doses(x))

    expect_true(is.numeric(empiric_tox_rate(x)))
    expect_equal(length(empiric_tox_rate(x)), num_doses(x))

    expect_true(is.logical(supports_sampling(x)))

    expect_error(prob_tox_samples(x))
    expect_error(prob_tox_samples(x, tall = TRUE))

    # Expect summary to not error. This is how that is tested, apparently:
    expect_error(summary(x), NA)
    expect_output(print(x))
    expect_true(tibble::is_tibble(as_tibble(x)))
    expect_true(nrow(as_tibble(x)) >= num_doses(x))

  })
