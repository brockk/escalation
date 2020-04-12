
test_that('num_dose_path_nodes is right', {

  x <- num_dose_path_nodes(num_patient_outcomes = 2, cohort_sizes = rep(3, 2))
  expect_equal(x, c(1, 4, 16))

  x <- num_dose_path_nodes(num_patient_outcomes = 4, cohort_sizes = rep(3, 2))
  expect_equal(x, c(1, 20, 400))
})
