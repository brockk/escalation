

test_that('CorrelatedPatientSample works like it should.', {

  ps <- CorrelatedPatientSample$new(num_patients = 10, tau = 2)

  expect_equal(
    ps$num_patients,
    10
  )
  expect_true(ps$can_grow)
  theta7 <- ps$get_theta(i = 7)
  t7 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_tox(i = 7, prob_tox = x)
  )
  e3 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_eff(i = 3, prob_eff = x)
  )
  # TODO At most one turn

  # Expand manually
  ps$expand_to(num_patients = 12)
  expect_equal(
    ps$num_patients,
    12
  )
  expect_true(ps$can_grow)
  # Expansion should not affect what was already known
  t7_v2 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_tox(i = 7, prob_tox = x)
  )
  expect_equal(t7, t7_v2)
  e3_v2 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_eff(i = 3, prob_eff = x)
  )
  expect_equal(e3, e3_v2)

  # Expand implicitly
  z <- ps$get_patient_eff(i = 15, 0.1)
  expect_equal(
    ps$num_patients,
    15
  )
  expect_true(ps$can_grow)
  # Expansion should not affect what was already known
  t7_v3 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_tox(i = 7, prob_tox = x)
  )
  expect_equal(t7, t7_v3)
  e3_v3 <- sapply(
    seq(0, 1, 0.1),
    function(x) ps$get_patient_eff(i = 3, prob_eff = x)
  )
  expect_equal(e3, e3_v3)


  # Extract tox_u and eff_u
  theta <- ps$theta
  expect_equal(
    theta7,
    theta[7]
  )

  # Set theta
  theta_new <- rnorm(n = 20, mean = 0, sd = 2)
  ps$set_theta(theta = theta_new)
  expect_equal(
    ps$num_patients,
    20
  )
  expect_false(ps$can_grow) # Expansion is now prevented
  # What was known is now erased
  theta7_v2 <- ps$get_theta(i = 7)
  expect_true(theta7 != theta7_v2)
  # Further expansion is now not possible
  expect_error(ps$expand_to(25))
  expect_error(ps$get_theta(i = 25))
  expect_error(ps$get_patient_tox(i = 25, prob_tox = 0.1))
  expect_error(ps$get_patient_eff(i = 25, prob_eff = 0.1))

  tox_events <- ps$get_patient_tox(i = seq_len(ps$num_patients), prob_tox = 0.5)
  eff_events <- ps$get_patient_eff(i = seq_len(ps$num_patients), prob_eff = 0.3)
  expect_gt(
    cor(tox_events, eff_events),
    0
  )

})
