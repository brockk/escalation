
test_that(".outcomes_to_arrays works OK", {

  num_doses <- c(3, 4)
  df <-
    bind_rows(
      tibble(
        patient = 1,
        cohort = 1,
        dose = list(c(2, 1)),
        tox = 0,
        eff = 0
      ),
      tibble(
        patient = 2,
        cohort = 2,
        dose = list(c(3, 1)),
        tox = 0,
        eff = 1
      ),
      tibble(
        patient = 3,
        cohort = 2,
        dose = list(c(3, 1)),
        tox = 0,
        eff = 1
      ),
      tibble(
        patient = 4,
        cohort = 3,
        dose = list(c(3, 2)),
        tox = 1,
        eff = 0
      )
    )
  df <- spruce_outcomes_df(df)
  z <- .outcomes_to_arrays(df, num_doses)
  expect_equal(
    sum(z$num_patients),
    nrow(df)
  )
  expect_equal(
    sum(z$num_tox),
    sum(df$tox)
  )
  expect_equal(
    sum(z$num_eff),
    sum(df$eff)
  )

})
