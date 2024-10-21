
test_that("", {

  # p.17 of https://odin.mdacc.tmc.edu/~yyuan/Software/BOIN/BOIN2.6_tutorial.pdf

  num_doses <- c(3, 4)
  target <- 0.25
  boin_fitter <- get_boin_comb(num_doses = num_doses, target = target)

  # Describing outcomes as character string
  # TODO

  # Describing outcomes as data.frame
  df <-
    bind_rows(
      tibble(
        patient = 1,
        cohort = 1,
        dose = list(c(1, 1)),
        tox = 0
      ),
      tibble(
        patient = 2,
        cohort = 1,
        dose = list(c(1, 1)),
        tox = 0
      ),
      tibble(
        patient = 3,
        cohort = 1,
        dose = list(c(1, 1)),
        tox = 0
      )
    )
  x <- fit(boin_fitter, df)
  expect_equal(
    recommended_dose(x),
    c(1, 2)
  )
  expect_true(
    continue(x)
  )
  # expect_output(
  #   print(x),
  #   "The model advocates continuing at dose '1.2'."
  # )
  # check_dose_selector_consistency(x)

  df <-
    bind_rows(
      tibble(
        patient = 1,
        cohort = 1,
        dose = list(c(1, 1)),
        tox = 0
      ),
      tibble(
        patient = 2,
        cohort = 1,
        dose = list(c(1, 1)),
        tox = 0
      ),
      tibble(
        patient = 3,
        cohort = 1,
        dose = list(c(1, 1)),
        tox = 0
      ),

      tibble(
        patient = 4,
        cohort = 2,
        dose = list(c(1, 2)),
        tox = 0
      ),
      tibble(
        patient = 5,
        cohort = 2,
        dose = list(c(1, 2)),
        tox = 0
      ),
      tibble(
        patient = 6,
        cohort = 3,
        dose = list(c(1, 2)),
        tox = 1
      )
    )
  x <- fit(boin_fitter, df)
  expect_equal(
    recommended_dose(x),
    c(1, 1)
  )
  expect_true(
    continue(x)
  )
  # expect_output(
  #   print(x),
  #   "The model advocates continuing at dose '1.1'."
  # )
  # check_dose_selector_consistency(x)



})
