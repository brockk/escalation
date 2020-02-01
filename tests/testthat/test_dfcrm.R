
test_that('dfcrm_dose_selector matches dfcrm', {

  # Example 1 - Empiric model, non-standard scale parameter
  skeleton <- c(0.1, 0.2, 0.4, 0.55)
  target <- 0.2
  scale = sqrt(0.75)
  outcomes <- '2NNT 2NNN 3NTT 2NNT'

  # dosefinding model
  model <- get_dfcrm(skeleton = skeleton, target = target, scale = scale)
  x <- model %>% fit(outcomes)

  # dfcrm model
  y <- dfcrm::crm(prior = skeleton, target = target, scale = scale,
                  tox = c(0,0,1, 0,0,0, 0,1,1, 0,0,1),
                  level = c(2,2,2, 2,2,2, 3,3,3, 2,2,2))

  expect_equal(recommended_dose(x), y$mtd)
  expect_equal(round(mean_prob_tox(x), 2),  round(y$ptox, 2))
  expect_equal(x$dfcrm_fit$model, 'empiric')
  expect_equal(x$dfcrm_fit$prior.var, 0.75)


  # Example 2 - Logit model, non-standard intercept parameter
  skeleton <- c(0.1, 0.2, 0.33, 0.45, 0.6, 0.7, 0.8)
  target <- 0.33
  outcomes <- '1NNN 2NNN 3NTT 2NNN 3TNN 3TNT 2NNN'

  # dosefinding model
  model <- get_dfcrm(skeleton = skeleton, target = target, intcpt = 4,
                     model = 'logistic')
  x <- model %>% fit(outcomes)


  # dfcrm model
  y <- dfcrm::crm(prior = skeleton, target = target, intcpt = 4,
                  model = 'logistic',
                  tox = c(0,0,0, 0,0,0, 0,1,1, 0,0,0, 1,0,0, 1,0,1, 0,0,0),
                  level = c(1,1,1, 2,2,2, 3,3,3, 2,2,2, 3,3,3, 3,3,3, 2,2,2))

  expect_equal(recommended_dose(x), y$mtd)
  expect_equal(round(mean_prob_tox(x), 2),  round(y$ptox, 2))
  expect_equal(x$dfcrm_fit$model, 'logistic')
  expect_equal(x$dfcrm_fit$intcpt, 4)
})
