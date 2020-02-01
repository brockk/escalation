test_that('BOIN recommendations match published example.', {

  num_doses <- 5
  target <- 0.3
  boin_fitter <- get_boin(num_doses = num_doses, target = target)

  x <- fit(boin_fitter, '1NNN')
  expect_equal(recommended_dose(x), 2)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT')
  expect_equal(recommended_dose(x), 2)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN')
  expect_equal(recommended_dose(x), 4)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT 3TNN')
  expect_equal(recommended_dose(x), 3)
  expect_true(continue(x))

  expect_equal(n_at_dose(x), c(3, 6, 18, 3, 0))
  expect_equal(tox_at_dose(x), c(0, 1, 5, 3, 0))
})
