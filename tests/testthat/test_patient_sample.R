
test_that('PatientSample works like it should.', {

  ps <- PatientSample$new(num_patients = 10)

  expect_equal(
    ps$num_patients,
    10
  )
  expect_true(ps$can_grow)
  tox_u7 <- ps$get_tox_u(i = 7)
  expect_equal(
    ps$get_patient_tox(i = 7, prob_tox = tox_u7 * 0.99),
    0
  )
  expect_equal(
    ps$get_patient_tox(i = 7, prob_tox = tox_u7 * 1.01),
    1
  )
  eff_u3 <- ps$get_eff_u(i = 3)
  expect_equal(
    ps$get_patient_eff(i = 3, prob_eff = eff_u3 * 0.99),
    0
  )
  expect_equal(
    ps$get_patient_eff(i = 3, prob_eff = eff_u3 * 1.01),
    1
  )

  # Expand manually
  ps$expand_to(num_patients = 12)
  expect_equal(
    ps$num_patients,
    12
  )
  expect_true(ps$can_grow)
  # Expansion should not affect what was already known
  tox_u7_v2 <- ps$get_tox_u(i = 7)
  expect_equal(
    tox_u7,
    tox_u7_v2
  )
  eff_u3_v2 <- ps$get_eff_u(i = 3)
  expect_equal(
    eff_u3,
    eff_u3_v2
  )

  # Expand implicitly
  z <- ps$get_patient_eff(i = 15, 0.1)
  expect_equal(
    ps$num_patients,
    15
  )
  expect_true(ps$can_grow)
  # Expansion should not affect what was already known
  tox_u7_v3 <- ps$get_tox_u(i = 7)
  expect_equal(
    tox_u7,
    tox_u7_v3
  )
  eff_u3_v3 <- ps$get_eff_u(i = 3)
  expect_equal(
    eff_u3,
    eff_u3_v3
  )

  # Extract tox_u and eff_u
  tox_u <- ps$tox_u
  expect_equal(
    tox_u7,
    tox_u[7]
  )
  eff_u <- ps$eff_u
  expect_equal(
    eff_u3,
    eff_u[3]
  )

  # Set tox_u and eff_u
  tox_u_new <- runif(n = 20)
  eff_u_new <- runif(n = 20)
  ps$set_eff_and_tox(tox_u = tox_u_new, eff_u = eff_u_new)
  expect_equal(
    ps$num_patients,
    20
  )
  expect_false(ps$can_grow) # Expansion is now prevented
  # What was known is now erased
  tox_u7_v4 <- ps$get_tox_u(i = 7)
  expect_true(tox_u7 != tox_u7_v4)
  eff_u3_v4 <- ps$get_eff_u(i = 3)
  expect_true(eff_u3 != eff_u3_v4)
  # Further expansion is now not possible
  expect_error(ps$expand_to(25))
  expect_error(ps$get_tox_u(i = 25))
  expect_error(ps$get_patient_tox(i = 25, prob_tox = 0.1))
  expect_error(ps$get_eff_u(i = 25))
  expect_error(ps$get_patient_eff(i = 25, prob_eff = 0.1))

})
