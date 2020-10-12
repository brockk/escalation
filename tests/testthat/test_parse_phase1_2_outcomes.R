# Test parse_phase1_2_outcomes.
# This function gets its own test script to keep things tidy.

# Test some cases that should work ----
# Regular case
test_that('parse_phase1_2_outcomes parses "" correctly', {
  x <- parse_phase1_2_outcomes('', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 0)
  expect_equal(length(x$dose), 0)
  expect_equal(length(x$eff), 0)
  expect_equal(length(x$tox), 0)
})

test_that('parse_phase1_2_outcomes parses "" correctly to list', {
  x <- parse_phase1_2_outcomes('', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 0)
  expect_equal(length(x$dose), 0)
  expect_equal(length(x$eff), 0)
  expect_equal(length(x$tox), 0)
})

test_that('parse_phase1_2_outcomes parses "1EEE 3TTT" correctly', {
  x <- parse_phase1_2_outcomes('1EEE 3TTT', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 6)
  expect_equal(x$dose, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$eff, c(1, 1, 1, 0, 0, 0))
  expect_equal(x$tox, c(0, 0, 0, 1, 1, 1))
})

test_that('parse_phase1_2_outcomes parses "1EEE 3TTT" correctly to list', {
  x <- parse_phase1_2_outcomes('1EEE 3TTT', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 6)
  expect_equal(x$dose, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$eff, c(1, 1, 1, 0, 0, 0))
  expect_equal(x$tox, c(0, 0, 0, 1, 1, 1))
})

# A regular case with no spaces; I do not recommend this because it is hard to
# read but it should work, nevertheless.
test_that('parse_phase1_2_outcomes parses "1E2T2N2E2E" correctly', {
  x <- parse_phase1_2_outcomes('1E2T2N2E2E', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 5)
  expect_equal(x$dose, c(1, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

test_that('parse_phase1_2_outcomes parses "1E2T2N2E2E" correctly to list', {
  x <- parse_phase1_2_outcomes('1E2T2N2E2E', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 5)
  expect_equal(x$dose, c(1, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

# Odd shaped cohorts
test_that('parse_phase1_2_outcomes parses "1ETT 2T 2NBENTE 3E" correctly', {
  x <- parse_phase1_2_outcomes('1ETT 2T 2NBENTE 3E', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('parse_phase1_2_outcomes parses "1ETT 2T 2NBENTE 3E" correctly to list', {
  x <- parse_phase1_2_outcomes('1ETT 2T 2NBENTE 3E', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Silly but valid dose-levels
test_that('parse_phase1_2_outcomes parses "96ETT 40T 1NBENTE 174E" correctly', {
  x <- parse_phase1_2_outcomes('96ETT 40T 1NBENTE 174E', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$dose, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('parse_phase1_2_outcomes parses "96ETT 40T 1NBENTE 174E" correctly to list', {
  x <- parse_phase1_2_outcomes('96ETT 40T 1NBENTE 174E', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$dose, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Leading white-space
test_that('parse_phase1_2_outcomes parses " 1ETT 2T 2NBENTE 2E" correctly', {
  x <- parse_phase1_2_outcomes(' 1ETT 2T 2NBENTE 2E', as_list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('parse_phase1_2_outcomes parses " 1ETT 2T 2NBENTE 2E" correctly to list', {
  x <- parse_phase1_2_outcomes(' 1ETT 2T 2NBENTE 2E', as_list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$dose, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Test some cases that should not work ----
# A string that is not welcome here
test_that('parse_phase1_2_outcomes parses "12ETT Nigel Farage" with error', {
  expect_error(parse_phase1_2_outcomes('12ETT Nigel Farage', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "12ETT Nigel Farage" to list with error', {
  expect_error(parse_phase1_2_outcomes('12ETT Nigel Farage', as_list = TRUE))
})

# Decimal dose-levels
test_that('parse_phase1_2_outcomes parses " 1ETT 2.0T 2NBENTE 2E" with error', {
  expect_error(parse_phase1_2_outcomes(' 1ETT 2.0T 2NBENTE 2E', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses " 1ETT 2.0T 2NBENTE 2E" to list with error', {
  expect_error(parse_phase1_2_outcomes(' 1ETT 2.0T 2NBENTE 2E', as_list = TRUE))
})

test_that('parse_phase1_2_outcomes parses ".1ETT 2T 2NBENTE 2E" with error', {
  expect_error(parse_phase1_2_outcomes('.1ETT 2T 2NBENTE 2E', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses ".1ETT 2T 2NBENTE 2E" to list with error', {
  expect_error(parse_phase1_2_outcomes('.1ETT 2T 2NBENTE 2E', as_list = TRUE))
})

# Negative dose-levels
test_that('parse_phase1_2_outcomes parses "12ETT 2T 2NBENTE -1E" with error', {
  expect_error(parse_phase1_2_outcomes('12ETT 2T 2NBENTE -1E', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "12ETT 2T 2NBENTE -1E" to list with error', {
  expect_error(parse_phase1_2_outcomes('12ETT 2T 2NBENTE -1E', as_list = TRUE))
})

test_that('parse_phase1_2_outcomes parses "-12ETT 2T 2NBENTE 1E" with error', {
  expect_error(parse_phase1_2_outcomes('-12ETT 2T 2NBENTE 1E', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "-12ETT 2T 2NBENTE 1E" to list with error', {
  expect_error(parse_phase1_2_outcomes('-12ETT 2T 2NBENTE 1E', as_list = TRUE))
})

test_that('parse_phase1_2_outcomes parses "12ETT 2T -2NBENTE 1E" with error', {
  expect_error(parse_phase1_2_outcomes('12ETT 2T -2NBENTE 1E', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "12ETT 2T -2NBENTE 1E" to list with error', {
  expect_error(parse_phase1_2_outcomes('12ETT 2T -2NBENTE 1E', as_list = TRUE))
})

# Zero dose-level
test_that('parse_phase1_2_outcomes parses "1T 0NN" with error', {
  expect_error(parse_phase1_2_outcomes('1T 0NN', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "1T 0NN" to list with error', {
  expect_error(parse_phase1_2_outcomes('1T 0NN', as_list = TRUE))
})

test_that('parse_phase1_2_outcomes parses "0ENTBENTB" with error', {
  expect_error(parse_phase1_2_outcomes('0ENTBENTB', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "0ENTBENTB" to list with error', {
  expect_error(parse_phase1_2_outcomes('0ENTBENTB', as_list = TRUE))
})

# Nothing but white-space
test_that('parse_phase1_2_outcomes parses " " with error', {
  expect_error(parse_phase1_2_outcomes(' ', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses " " to list with error', {
  expect_error(parse_phase1_2_outcomes(' ', as_list = TRUE))
})

# Looks plausible
test_that('parse_phase1_2_outcomes parses "1ET TNB" with error', {
  expect_error(parse_phase1_2_outcomes('1ET TNB', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "1ET TNB" to list with error', {
  expect_error(parse_phase1_2_outcomes('1ET TNB', as_list = TRUE))
})

test_that('parse_phase1_2_outcomes parses "1ET T3NB" with error', {
  expect_error(parse_phase1_2_outcomes('1ET T3NB', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "1ET T3NB" to list with error', {
  expect_error(parse_phase1_2_outcomes('1ET T3NB', as_list = TRUE))
})

test_that('parse_phase1_2_outcomes parses "1ET 3TNB 4" with error', {
  expect_error(parse_phase1_2_outcomes('1ET 3TNB 4', as_list = FALSE))
})

test_that('parse_phase1_2_outcomes parses "1ET 3TNB 4" to list with error', {
  expect_error(parse_phase1_2_outcomes('1ET 3TNB 4', as_list = TRUE))
})
