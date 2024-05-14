
test_that('tpi_selector matches published example.', {

  # Reproduce decisions conveyed in Table 1 in Ji et al. (2007)
  # at https://doi.org/10.1177/1740774507079442

  num_doses <- 5
  target <- 0.3

  model <- get_tpi(num_doses = num_doses, target = target,
                   k1 = 1, k2 = 1.5, exclusion_certainty = 0.95)



  # Tests at dose 1 ----

  # Three patients treated
  fit <- model %>% fit('1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  # Six patients treated
  fit <- model %>% fit('1NNN 1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  # Nine patients treated
  fit <- model %>% fit('1NNN 1NNN 1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNT 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NTT 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1TTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  # Twelve patients treated
  fit <- model %>% fit('1NNN 1NNN 1NNN 1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NNN 1NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NNN 1NTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NNN 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NNT 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1NTT 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNN 1TTT 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NNT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1NTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNN 1TTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NNT 1TTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1NTT 1TTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('1TTT 1TTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_output(
    print(fit),
    "The model advocates stopping and recommending no dose."
  )
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))
  check_dose_selector_consistency(fit)



  # Tests at dose 2 ----

  # Three patients treated
  fit <- model %>% fit('2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  # Six patients treated
  fit <- model %>% fit('2NNN 2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  # Nine patients treated
  fit <- model %>% fit('2NNN 2NNN 2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NNT')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2TTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNT 2TTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2TTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  # Twelve patients treated
  fit <- model %>% fit('2NNN 2NNN 2NNN 2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NNN 2NNT')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NNN 2NTT')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NNN 2TTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NNT 2TTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2NTT 2TTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNN 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NNT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2NTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNN 2TTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NNT 2TTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2NTT 2TTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('2TTT 2TTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 1."
  )
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  check_dose_selector_consistency(fit)



  # Tests at top dose (i.e. escalation impossible) ----

  # Three patients treated
  fit <- model %>% fit('5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  # Six patients treated
  fit <- model %>% fit('5NNN 5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  # Nine patients treated
  fit <- model %>% fit('5NNN 5NNN 5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5TTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNT 5TTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5TTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  # Twelve patients treated
  fit <- model %>% fit('5NNN 5NNN 5NNN 5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NNN 5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NNN 5NTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NNN 5TTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NNT 5TTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5NTT 5TTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 5."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNN 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NNT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5NTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNN 5TTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NNT 5TTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5NTT 5TTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

  fit <- model %>% fit('5TTT 5TTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_output(
    print(fit),
    "The model advocates continuing at dose 4."
  )
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  check_dose_selector_consistency(fit)

})

test_that('tpi_selector supports correct interface.', {

  num_doses <- 5
  target <- 0.3

  model_fitter <- get_tpi(num_doses = num_doses, target = target,
                          k1 = 1, k2 = 1.5, exclusion_certainty = 0.9)

  # Example 1, using outcome string
  x <- fit(model_fitter, '1NNN 2NTT')

  expect_equal(tox_target(x), 0.3)
  expect_true(is.numeric(tox_target(x)))

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

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

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

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))


  # Example 2, using trivial outcome string
  x <- fit(model_fitter, '')

  expect_equal(tox_target(x), 0.3)
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

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  mf <- model_frame(x)
  expect_equal(nrow(mf), 0)
  expect_equal(ncol(mf), 4)

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

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

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))


  # Example 3, using tibble
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0, 0,0, 1,1)
  )
  x <- fit(model_fitter, outcomes)

  expect_equal(tox_target(x), 0.3)
  expect_true(is.numeric(tox_target(x)))

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

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

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

  expect_error(prob_tox_samples(x))
  expect_error(prob_tox_samples(x, tall = TRUE))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))

})

test_that('tpi_selector respects suspended doses', {

  model <- get_tpi(num_doses = 5, target = 0.3, k1 = 1, k2 = 1.5,
                        exclusion_certainty = 0.7)


  fit <- model %>% fit('2N')
  expect_equal(fit %>% recommended_dose(), 3)
  expect_true(fit %>% continue())
  expect_output(
    print(fit),
    "The model advocates continuing at dose 3."
  )
  expect_equal(fit %>% dose_admissible(), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('3TTT')
  expect_equal(fit %>% recommended_dose(), 2)
  expect_true(fit %>% continue())
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(fit %>% dose_admissible(), c(TRUE, TRUE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('3TTT 2N')
  expect_equal(fit %>% recommended_dose(), 2)
  expect_true(fit %>% continue())
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(fit %>% dose_admissible(), c(TRUE, TRUE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('3TTT 2N 1N')
  expect_equal(fit %>% recommended_dose(), 2)
  expect_true(fit %>% continue())
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(fit %>% dose_admissible(), c(TRUE, TRUE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('3TTT 2N 1N 2NNNNNNNNNNNNNNNNNNN')
  expect_equal(fit %>% recommended_dose(), 2)
  expect_true(fit %>% continue())
  expect_output(
    print(fit),
    "The model advocates continuing at dose 2."
  )
  expect_equal(fit %>% dose_admissible(), c(TRUE, TRUE, FALSE, FALSE, FALSE))

})
