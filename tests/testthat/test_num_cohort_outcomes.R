
test_that('num_cohort_outcomes is right', {
  x <- num_cohort_outcomes(num_patient_outcomes = 2, cohort_size = 3)
  expect_equal(x, 4)

  x <- num_cohort_outcomes(num_patient_outcomes = 4, cohort_size = 3)
  expect_equal(x, 20)
})
