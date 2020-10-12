
# The empty string should not fail
test_that('parse_phase1_outcomes "" correctly', {
  x <- parse_phase1_outcomes('', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 0)
})

test_that('parse_phase1_outcomes parses "" correctly to list', {
  x <- parse_phase1_outcomes('', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 0)
  expect_equal(x$dose, integer(length = 0))
  expect_equal(x$tox, integer(length = 0))
})

# Regular example
test_that('parse_phase1_outcomes parses "1NNN 3NTT" correctly', {
  x <- parse_phase1_outcomes('1NNN 3NTT', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 6)
  expect_equal(x$dose, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$tox, c(0, 0, 0, 0, 1, 1))
})

test_that('parse_phase1_outcomes parses "1NNN 3NTT" correctly to list', {
  x <- parse_phase1_outcomes('1NNN 3NTT', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 6)
  expect_equal(x$dose, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$tox, c(0, 0, 0, 0, 1, 1))
})

# A regular case with no spaces; I do not recommend this because it is hard to
# read but it should work, nevertheless.
test_that('parse_phase1_outcomes parses "1N2T2N2N2N" correctly', {
  x <- parse_phase1_outcomes('1N2T2N2N2N', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 5)
  expect_equal(x$dose, c(1, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

test_that('parse_phase1_outcomes parses "1N2T2N2N2N" correctly to list', {
  x <- parse_phase1_outcomes('1N2T2N2N2N', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 5)
  expect_equal(x$dose, c(1, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

# n=1 example
test_that('parse_phase1_outcomes parses "5T" correctly', {
  x <- parse_phase1_outcomes('5T', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 1)
  expect_equal(x$dose, c(5))
  expect_equal(x$tox, c(1))
})

test_that('parse_phase1_outcomes parses "5T" correctly to  list', {
  x <- parse_phase1_outcomes('5T', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 1)
  expect_equal(x$dose, c(5))
  expect_equal(x$tox, c(1))
})

# Odd shaped cohorts
test_that('parse_phase1_outcomes parses "1NTT 2T 2NTNNTN 3N" correctly', {
  x <- parse_phase1_outcomes('1NTT 2T 2NTNNTN 3N', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('parse_phase1_outcomes parses "1NTT 2T 2NTNNTN 3N" correctly to list', {
  x <- parse_phase1_outcomes('1NTT 2T 2NTNNTN 3N', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Silly but valid dose-levels
test_that('parse_phase1_outcomes parses "96NTT 40T 1NTNNTN 174N" correctly', {
  x <- parse_phase1_outcomes('96NTT 40T 1NTNNTN 174N', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$dose, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('parse_phase1_outcomes parses "96NTT 40T 1NTNNTN 174N" correctly to list', {
  x <- parse_phase1_outcomes('96NTT 40T 1NTNNTN 174N', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$dose, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Leading white-space
test_that('parse_phase1_outcomes parses " 1NTT 2T 2NTNNTN 2N" correctly', {
  x <- parse_phase1_outcomes(' 1NTT 2T 2NTNNTN 2N', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('parse_phase1_outcomes parses " 1NTT 2T 2NTNNTN 2N" correctly to list', {
  x <- parse_phase1_outcomes(' 1NTT 2T 2NTNNTN 2N', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Test some cases that should not work
# A string that is not welcome here
test_that('parse_phase1_outcomes parses "12NTT Nigel Farage" with error', {
  expect_error(parse_phase1_outcomes('12NTT Nigel Farage', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "12NTT Nigel Farage" to list with error', {
  expect_error(parse_phase1_outcomes('12NTT Nigel Farage', as_list = TRUE))
})

# Decimal dose-levels
test_that('parse_phase1_outcomes parses " 1NTT 2.0T 2NTNNTN 2N" with error', {
  expect_error(parse_phase1_outcomes(' 1NTT 2.0T 2NTNNTN 2N', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses " 1NTT 2.0T 2NTNNTN 2N" to list with error', {
  expect_error(parse_phase1_outcomes(' 1NTT 2.0T 2NTNNTN 2N', as_list = TRUE))
})

test_that('parse_phase1_outcomes parses ".1NTT 2T 2NTNNTN 2N" with error', {
  expect_error(parse_phase1_outcomes('.1NTT 2T 2NTNNTN 2N', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses ".1NTT 2T 2NTNNTN 2N" to list with error', {
  expect_error(parse_phase1_outcomes('.1NTT 2T 2NTNNTN 2N', as_list = TRUE))
})

# Negative dose-levels
test_that('parse_phase1_outcomes parses "12NTT 2T 2NTNNTN -1N" with error', {
  expect_error(parse_phase1_outcomes('12NTT 2T 2NTNNTN -1N', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "12ETT 2T 2NTNNTN -1N" to list with error', {
  expect_error(parse_phase1_outcomes('12ETT 2T 2NTNNTN -1N', as_list = TRUE))
})

test_that('parse_phase1_outcomes parses "-12NTT 2T 2NTNNTN 1N" with error', {
  expect_error(parse_phase1_outcomes('-12NTT 2T 2NTNNTN 1N', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "-12NTT 2T 2NTNNTN 1N" to list with error', {
  expect_error(parse_phase1_outcomes('-12NTT 2T 2NTNNTN 1N', as_list = TRUE))
})

test_that('parse_phase1_outcomes parses "12NTT 2T -2NTNNTN 1N" with error', {
  expect_error(parse_phase1_outcomes('12NTT 2T -2NTNNTN 1N', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "12NTT 2T -2NTNNTN 1N" to list with error', {
  expect_error(parse_phase1_outcomes('12NTT 2T -2NTNNTN 1N', as_list = TRUE))
})

# Zero dose-level
test_that('parse_phase1_outcomes parses "1T 0NN" with error', {
  expect_error(parse_phase1_outcomes('1T 0NN', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "1T 0NN" to list with error', {
  expect_error(parse_phase1_outcomes('1T 0NN', as_list = TRUE))
})

test_that('parse_phase1_outcomes parses "0NNTTNNTT" with error', {
  expect_error(parse_phase1_outcomes('0NNTTNNTT', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "0NNTTNNTT" to list with error', {
  expect_error(parse_phase1_outcomes('0NNTTNNTT', as_list = TRUE))
})

# Nothing but white-space
test_that('parse_phase1_outcomes parses " " with error', {
  expect_error(parse_phase1_outcomes(' ', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses " " to list with error', {
  expect_error(parse_phase1_outcomes(' ', as_list = TRUE))
})

# Looks plausible
test_that('parse_phase1_outcomes parses "1NT TNT" with error', {
  expect_error(parse_phase1_outcomes('1NT TNT', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "1NT TNT" to list with error', {
  expect_error(parse_phase1_outcomes('1NT TNT', as_list = TRUE))
})

test_that('parse_phase1_outcomes parses "1NT T3NT" with error', {
  expect_error(parse_phase1_outcomes('1NT T3NT', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "1NT T3NT" to list with error', {
  expect_error(parse_phase1_outcomes('1NT T3NT', as_list = TRUE))
})

test_that('parse_phase1_outcomes parses "1NT 3TNT 4" with error', {
  expect_error(parse_phase1_outcomes('1NT 3TNT 4', as_list = FALSE))
})

test_that('parse_phase1_outcomes parses "1NT 3TNT 4" to list with error', {
  expect_error(parse_phase1_outcomes('1NT 3TNT 4', as_list = TRUE))
})
