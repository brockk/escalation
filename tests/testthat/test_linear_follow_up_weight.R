
test_that('linear_follow_up_weight works like it should.', {

  expect_equal(
    linear_follow_up_weight(
      now_time = 10,
      recruited_time = 4:7,
      tox = c(0, 0, 0, 1),
      max_time = 6,
      tox_has_weight_1 = TRUE
    ),
    c(1.0000000, 0.8333333, 0.6666667, 1.0000000),
    tolerance = 0.001
  )

  expect_equal(
    linear_follow_up_weight(
      now_time = 10,
      recruited_time = 4:7,
      tox = c(0, 0, 0, 1),
      max_time = 6,
      tox_has_weight_1 = FALSE
    ),
    c(1.0000000, 0.8333333, 0.6666667, 0.5),
    tolerance = 0.001
  )

})
