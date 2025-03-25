
#' Check the consistency of a dose_selector instance
#'
#' @param x simulations object
#' @importFrom testthat expect_is expect_no_error expect_output
check_simulations_consistency <- function(x) {

  expect_is(x, "simulations")

  expect_no_error({
    num_patients(x)
  })
  expect_no_error({
    num_doses(x)
  })
  expect_no_error({
    dose_indices(x)
  })
  expect_no_error({
    dose_strings(x)
  })
  suppressWarnings({
    expect_no_error({
      doses_given(x, dose_string = TRUE)
    })
  })
  suppressWarnings({
    expect_no_error({
      doses_given(x, dose_string = FALSE)
    })
  })
  expect_no_error({
    recommended_dose(x, dose_string = TRUE)
  })
  expect_no_error({
    recommended_dose(x, dose_string = FALSE)
  })
  expect_no_error({
    n_at_dose(x)
  })
  expect_no_error({
    n_at_dose(x, dose = "recommended")
  })
  expect_no_error({
    n_at_recommended_dose(x)
  })
  expect_no_error({
    tox_at_dose(x)
  })
  expect_no_error({
    num_tox(x)
  })
  expect_no_error({
    eff_at_dose(x)
  })
  expect_no_error({
    num_eff(x)
  })
  expect_no_error({
    prob_recommend(x)
  })
  expect_no_error({
    prob_administer(x, method = 0)
  })
  expect_no_error({
    prob_administer(x, method = 1)
  })
  expect_no_error({
    trial_duration(x)
  })
  expect_no_error({
    expect_output(print(x))
  })
  expect_no_error({
    summary(x)
  })
  expect_no_error({
    as_tibble(x)
  })

}
