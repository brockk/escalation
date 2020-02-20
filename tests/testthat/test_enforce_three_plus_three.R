
test_that('3+3 enforcer errors when it should.', {

  expect_error(enforce_three_plus_three(outcomes = '1NNN 1N'))

  expect_error(enforce_three_plus_three(outcomes = '1NNN 1NNN'))

  expect_error(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NNN 4N'))

  expect_error(enforce_three_plus_three(outcomes = '1NNN 2TTT 1N'))

  # outcomes = '1NTT 1N' # does not fail
  # outcomes = '1NNN 1T' # does not fail
})


test_that('3+3 enforcer acquiesces when it should.', {

  expect_null(enforce_three_plus_three(outcomes = ''))

  expect_null(enforce_three_plus_three(outcomes = '1N'))

  expect_null(enforce_three_plus_three(outcomes = '1T'))

  expect_null(enforce_three_plus_three(outcomes = '1NN'))

  expect_null(enforce_three_plus_three(outcomes = '1NT'))

  expect_null(enforce_three_plus_three(outcomes = '1TT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNT'))

  expect_null(enforce_three_plus_three(outcomes = '1NTT'))

  expect_null(enforce_three_plus_three(outcomes = '1TTT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2N'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2T'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2TT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NTT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2TTT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2N'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2T'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2TT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NNN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NNT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NTT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNT 2NNN'))

  # Skip cohorts 3 and 4 to check behaviour at high doses
  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5N'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNN'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5NTT'))

  expect_null(enforce_three_plus_three(outcomes = '1NNN 2NNN 3NNN 4NNN 5TTT'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5N'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NN'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NT'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TT'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NNN'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NNT'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5NTT'))

  expect_null(enforce_three_plus_three(
    outcomes = '1NNN 2NNN 3NNN 4NNN 5NNT 5TTT'))
})
