
test_that("get_boin_comb works as expected", {

  # p.17 of https://odin.mdacc.tmc.edu/~yyuan/Software/BOIN/BOIN2.6_tutorial.pdf

  num_doses <- c(3, 4)
  target <- 0.25
  boin_fitter <- get_boin_comb(num_doses = num_doses, target = target)

  # Describing outcomes as character string
  outcomes <- "1.1NNN"
  set.seed(2024)
  x <- fit(boin_fitter, outcomes)
  expect_equal(
    recommended_dose(x),
    # This is randomly oscillating because that is what the authors chose.
    c(1, 2)
    # c(2, 1)
  )
  expect_true(
    continue(x)
  )
  expect_output(
    print(x),
    "The model advocates continuing at dose 1.2"
  )
  # check_dose_selector_consistency(x)

  # Describing outcomes as data.frame
  outcomes <-
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
  set.seed(2024)
  x <- fit(boin_fitter, outcomes)
  expect_equal(
    recommended_dose(x),
    # This is randomly oscillating because that is what the authors chose.
    c(1, 2)
    # c(2, 1)
  )
  expect_true(
    continue(x)
  )
  expect_output(
    print(x),
    "The model advocates continuing at dose 1.2."
  )
  # check_dose_selector_consistency(x)


  outcomes <- "1.1NNN 1.2NNT"
  set.seed(2024)
  x <- fit(boin_fitter, outcomes)
  expect_equal(
    recommended_dose(x),
    c(1, 1)
  )
  expect_true(
    continue(x)
  )
  expect_output(
    print(x),
    "The model advocates continuing at dose 1.1."
  )
  # check_dose_selector_consistency(x)

  outcomes <-
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
  set.seed(2024)
  x <- fit(boin_fitter, outcomes)
  expect_equal(
    recommended_dose(x),
    c(1, 1)
  )
  expect_true(
    continue(x)
  )
  expect_output(
    print(x),
    "The model advocates continuing at dose 1.1."
  )
  # check_dose_selector_consistency(x)

  # Describing outcomes as character string
  outcomes <- "1.1NNN"
  set.seed(2024)
  x <- fit(boin_fitter, outcomes)


  # Check it is possible to override p.saf and p.tox
  num_doses <- c(3, 5)
  target <- 0.3
  boin_fitter1 <- get_boin_comb(
    num_doses = num_doses, target = target,
    p.saf = 0.18, p.tox = 0.42
  )
  boin_fitter2 <- get_boin_comb(
    num_doses = num_doses, target = target,
    p.saf = 0.01, p.tox = 0.99
  )

  outcomes <- "1.1NNN 2.1NNNNNNT 2.2NNNNNT"
  set.seed(2025)
  x1 <- fit(boin_fitter1, outcomes)
  # First design should escalate:
  expect_equal(
    recommended_dose(x1),
    c(2, 3)
  )

  set.seed(2025)
  x2 <- fit(boin_fitter2, outcomes)
  # Second design should stick:
  expect_equal(
    recommended_dose(x2),
    c(2, 2)
  )

})
