
test_that("dose_vector_to_string works OK", {

  expect_error(dose_vector_to_string(c(1, 3)))
  expect_equal(
    dose_vector_to_string(as.integer(c(1, 3))),
    "1.3"
  )
  expect_equal(
    dose_vector_to_string(as.integer(c(3, 2))),
    "3.2"
  )

})

test_that("dose_string_to_vector works OK", {

  expect_no_error(dose_string_to_vector("1.3"))
  expect_equal(
    dose_string_to_vector("1.3"),
    c(1, 3)
  )
  expect_equal(
    dose_string_to_vector("3.2"),
    c(3, 2)
  )

})
