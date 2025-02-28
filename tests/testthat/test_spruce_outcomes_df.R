
test_that('Simulation results are sensible', {

  # Vanilla tox-only monotherapy dose-escalation example
  df <- tibble(
    cohort = c(1, 1, 1),
    patient = c(1, 2, 3),
    dose = c(1, 1, 1),
    tox = c(0, 0, 0)
  )
  abc = spruce_outcomes_df(df)
  expect_true(is.integer(abc$cohort))
  expect_true(is.integer(abc$patient))
  expect_true(is.integer(abc$dose))
  expect_true(is.integer(abc$tox))

  # Add time for time-to-event example
  df <- tibble(
    cohort = c(1, 1, 1),
    patient = c(1, 2, 3),
    dose = c(1, 1, 1),
    tox = c(0, 0, 0),
    time = c(0.1, 0.2, 0.3)
  )
  abc = spruce_outcomes_df(df)
  expect_true(is.integer(abc$cohort))
  expect_true(is.integer(abc$patient))
  expect_true(is.integer(abc$dose))
  expect_true(is.integer(abc$tox))
  expect_true(is.numeric(abc$time))

  # Add efficacy for EffTox-like example
  df <- tibble(
    cohort = c(1, 1, 1),
    patient = c(1, 2, 3),
    dose = c(1, 1, 1),
    tox = c(0, 0, 0),
    eff = c(1, 0, 1),
    time = c(0.1, 0.2, 0.3)
  )
  abc = spruce_outcomes_df(df)
  expect_true(is.integer(abc$cohort))
  expect_true(is.integer(abc$patient))
  expect_true(is.integer(abc$dose))
  expect_true(is.integer(abc$tox))
  expect_true(is.integer(abc$eff))
  expect_true(is.numeric(abc$time))

  # Investigate eff and tox in a TITE dose-combination example (all the bells):
  df <- tibble(
    cohort = c(1, 1, 1),
    patient = c(1, 2, 3),
    dose = list(c(1, 1), c(1, 1), c(1, 2)),
    dose_string = c("1.1", "1.1", "1.2"),
    tox = c(0, 0, 0),
    eff = c(1, 0, 1),
    time = c(0.1, 0.2, 0.3)
  )
  abc = spruce_outcomes_df(df)
  expect_true(is.integer(abc$cohort))
  expect_true(is.integer(abc$patient))
  expect_true(is.list(abc$dose))
  expect_true(is.character(abc$dose_string))
  expect_true(is.integer(abc$tox))
  expect_true(is.integer(abc$eff))
  expect_true(is.numeric(abc$time))

})
