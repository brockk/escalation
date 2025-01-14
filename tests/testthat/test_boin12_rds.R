
test_that("boin12_rds works as expected.", {

  # Recreate Table 3 of Lin, R., Zhou, Y., Yan, F., Li, D., & Yuan, Y. (2020).
  # BOIN12: Bayesian optimal interval phase I/II trial design for utility-based
  # dose finding in immunotherapy and targeted therapies.
  # JCO Precision Oncology, 4, 1393-1402.

  x <- boin12_rds(
    sample_sizes = c(0, 3, 6, 9),
    phi_t = 0.35,
    phi_e = 0.25,
    u1 = 100,
    u2 = 40,
    u3 = 60,
    u4 = 0,
    c_t = 0.95,
    c_e = 0.9,
    prior_alpha = 1,
    prior_beta = 1
  )

  # Test some selected scores
  expect_equal(
    x %>%
      filter(Patients == 0 & Toxicity == 0 & Efficacy == 0) %>%
      pull(RDS_x),
    60
  )
  expect_equal(
    x %>%
      filter(Patients == 3 & Toxicity == 0 & Efficacy == 3) %>%
      pull(RDS_x),
    91
  )
  expect_equal(
    x %>%
      filter(Patients == 6 & Toxicity == 1 & Efficacy == 5) %>%
      pull(RDS_x),
    87
  )
  expect_equal(
    x %>%
      filter(Patients == 6 & Toxicity == 5 & Efficacy == 1) %>%
      pull(RDS_x),
    as.numeric(NA)
  )
  expect_equal(
    x %>%
      filter(Patients == 9 & Toxicity == 0 & Efficacy == 8) %>%
      pull(RDS_x),
    99
  )
  expect_equal(
    x %>%
      filter(Patients == 9 & Toxicity == 2 & Efficacy == 3) %>%
      pull(RDS_x),
    32.5 # There is a tie with 9/5/5
  )
  expect_equal(
    x %>%
      filter(Patients == 9 & Toxicity == 5 & Efficacy == 5) %>%
      pull(RDS_x),
    32.5 # There is a tie with 9/2/3
  )
  expect_equal(
    x %>%
      filter(Patients == 9 & Toxicity == 3 & Efficacy == 1) %>%
      pull(RDS_x),
    7
  )
  expect_equal(
    x %>%
      filter(Patients == 9 & Toxicity == 4 & Efficacy == 0) %>%
      pull(RDS_x),
    as.numeric(NA)
  )
  expect_equal(
    x %>%
      filter(Patients == 9 & Toxicity == 6 & Efficacy == 9) %>%
      pull(RDS_x),
    as.numeric(NA)
  )

  # Change the utilities and all the rankings should change:
  x <- boin12_rds(
    sample_sizes = c(0, 3, 6, 9),
    phi_t = 0.35,
    phi_e = 0.25,
    u1 = 100,
    u2 = 50,
    u3 = 55,
    u4 = 0,
    c_t = 0.95,
    c_e = 0.9,
    prior_alpha = 1,
    prior_beta = 1
  )
  expect_true(
    x %>%
      filter(Patients == 0 & Toxicity == 0 & Efficacy == 0) %>%
      pull(RDS_x) !=
    60
  )
  expect_true(
    x %>%
      filter(Patients == 3 & Toxicity == 0 & Efficacy == 3) %>%
      pull(RDS_x) !=
    91
  )
  # Etc

})
