
test_that('wages_tair_selector does what is should.', {

  tox_skeleton = c(0.08, 0.15, 0.22, 0.29)
  eff_skeletons = matrix(nrow = 7, ncol = length(tox_skeleton))
  eff_skeletons[1,] <- c(0.60, 0.50, 0.40, 0.30)
  eff_skeletons[2,] <- c(0.50, 0.60, 0.50, 0.40)
  eff_skeletons[3,] <- c(0.40, 0.50, 0.60, 0.50)
  eff_skeletons[4,] <- c(0.30, 0.40, 0.50, 0.60)
  eff_skeletons[5,] <- c(0.40, 0.50, 0.60, 0.60)
  eff_skeletons[6,] <- c(0.50, 0.60, 0.60, 0.60)
  eff_skeletons[7,] <- c(rep(0.60, length(tox_skeleton)))
  # Bias towards second skeleton
  eff_skeleton_weights <- c(1, 2, 1, 1, 1, 1, 1)
  tox_limit = 0.5
  eff_limit = 0.5

  # Randomise to n=9
  model <- get_wages_and_tait(tox_skeleton = tox_skeleton,
                              eff_skeletons = eff_skeletons,
                              tox_limit = tox_limit,
                              eff_limit = eff_limit,
                              num_randomise = 9)

  fit <- model %>% fit('1NNN')
  expect_true(is_randomising(fit))
  expect_equal(sum(prob_administer(fit)), 1)

  prob_rand <- prob_administer(fit)
  # Test adaptive rand uses correct probability
  set.seed(123)
  d_ <- sapply(1:5000, function(i) recommended_dose(fit))
  prob_rand_ <- sapply(1:num_doses(fit), function(i) mean(d_ == i))
  expect_true(all(abs(prob_rand - prob_rand_) < 0.01))

  # Fit larger model
  outcomes <- '1NNN 3NTN 4TNN'
  fit <- model %>% fit(outcomes)
  expect_false(is_randomising(fit))
  expect_equal(sum(prob_administer(fit)), 1)
  df <- parse_phase1_2_outcomes(outcomes = outcomes, as_list = FALSE)
  # Recreate model fits
  tox_fit <- dfcrm::crm(prior = tox_skeleton, target = 1,
                        tox = df$tox, level = df$dose, model = 'empiric')
  epsilon <- 0.01
  expect_true(all(abs(tox_fit$ptox - fit %>% mean_prob_tox()) < epsilon))
  eff_fits <- lapply(
    1:nrow(eff_skeletons),
    function(i) dfcrm::crm(prior = eff_skeletons[i, ], target = 1,
                           tox = df$eff, level = df$dose, model = 'empiric')
  )
  expect_true(all(abs(eff_fits[[fit$eff_model_index]]$ptox -
                        fit %>% mean_prob_eff()) < epsilon))

  # Check stopping for tox fires
  fit <- model %>% fit('3T')
  expect_true(fit %>% continue())
  expect_gt(sum(dose_admissible(fit)), 0)

  fit <- model %>% fit('3TTT')
  expect_false(fit %>% continue())
  expect_equal(sum(dose_admissible(fit)), 0)


  # Check stopping for eff fires
  # Should not stop because not enough info to say inefficiacious yet:
  fit <- model %>% fit('1NNN 2NNN 3NNN 4NNN')
  expect_true(fit %>% continue())
  expect_false(is.na(recommended_dose(fit)))
  expect_gt(sum(dose_admissible(fit)), 0)

  # This should stop - 5 at each dose is enough to know that 0.5 is not likely
  outcomes <- '1NNNNNN 2NNNNNN 3NNNNNN 4NNNNNN'
  fit <- model %>% fit(outcomes)
  expect_false(fit %>% continue())
  expect_true(is.na(recommended_dose(fit)))

  # However, a model with huge randomisation stage should not stop under those
  # same outcomes:
  model <- get_wages_and_tait(tox_skeleton = tox_skeleton,
                              eff_skeletons = eff_skeletons,
                              tox_limit = tox_limit,
                              eff_limit = eff_limit,
                              num_randomise = 100)
  outcomes <- '1NNNNNN 2NNNNNN 3NNNNNN 4NNNNNN'
  fit <- model %>% fit(outcomes)
  expect_true(fit %>% continue())
  expect_false(is.na(recommended_dose(fit)))
  expect_gt(sum(dose_admissible(fit)), 0)

})

test_that('wages_tair_selector supports correct interface.', {

  tox_skeleton = c(0.08, 0.15, 0.22, 0.29, 0.36)
  eff_skeletons = matrix(nrow = 9, ncol = length(tox_skeleton))
  eff_skeletons[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20)
  eff_skeletons[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30)
  eff_skeletons[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40)
  eff_skeletons[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50)
  eff_skeletons[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60)
  eff_skeletons[6,] <- c(0.30, 0.40, 0.50, 0.60, 0.60)
  eff_skeletons[7,] <- c(0.40, 0.50, 0.60, 0.60, 0.60)
  eff_skeletons[8,] <- c(0.50, 0.60, 0.60, 0.60, 0.60)
  eff_skeletons[9,] <- c(rep(0.60, length(tox_skeleton)))
  eff_skeleton_weights = rep(1, nrow(eff_skeletons))
  tox_limit = 0.33
  eff_limit = 0.05

  model_fitter <- get_wages_and_tait(tox_skeleton = tox_skeleton,
                                     eff_skeletons = eff_skeletons,
                                     tox_limit = tox_limit,
                                     eff_limit = eff_limit,
                                     num_randomise = 16)



  # Example 1, using outcome string
  x <- fit(model_fitter, '1NEN 2NBT')

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.33)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.05)
  expect_true(is.numeric(eff_limit(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), unname(c(1,1,1, 2,2,2)))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), c(0,0,0, 0,1,1))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_true(is.numeric(weight(x)))
  expect_equal(length(weight(x)), num_patients(x))

  expect_equal(num_tox(x), 2)
  expect_true(is.integer(num_tox(x)))

  expect_equal(eff(x), c(0,1,0, 0,1,0))
  expect_true(is.integer(eff(x)))
  expect_equal(length(eff(x)), num_patients(x))

  expect_equal(num_eff(x), 2)
  expect_true(is.integer(num_eff(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1),
                                               eff = c(0,1,0,0,1,0))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(tox(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 3)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), TRUE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_equal(eff_at_dose(x), c(1,1,0,0,0))
  expect_true(is.integer(eff_at_dose(x)))
  expect_equal(length(eff_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(empiric_eff_rate(x)))
  expect_equal(length(empiric_eff_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_eff(x)))
  expect_equal(length(mean_prob_eff(x)), num_doses(x))

  expect_true(is.numeric(median_prob_eff(x)))
  expect_equal(length(median_prob_eff(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.numeric(prob_eff_quantile(x, p = 0.9)))
  expect_equal(length(prob_eff_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_eff_exceeds(x, 0.5)))
  expect_equal(length(prob_eff_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_true(is.data.frame(prob_tox_samples(x)))
  expect_true(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  expect_true(is.data.frame(prob_eff_samples(x)))
  expect_true(is.data.frame(prob_eff_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))



  # Example 2, using trivial outcome string
  x <- fit(model_fitter, '')

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.33)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.05)
  expect_true(is.numeric(eff_limit(x)))

  expect_equal(num_patients(x), 0)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), integer(length = 0))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), integer(length = 0))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), integer(length = 0))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_true(is.numeric(weight(x)))
  expect_equal(length(weight(x)), num_patients(x))

  expect_equal(num_tox(x), 0)
  expect_true(is.integer(num_tox(x)))

  expect_equal(eff(x), integer(length = 0))
  expect_true(is.integer(eff(x)))
  expect_equal(length(eff(x)), num_patients(x))

  expect_equal(num_eff(x), 0)
  expect_true(is.integer(num_eff(x)))

  mf <- model_frame(x)
  expect_equal(nrow(mf), 0)
  expect_equal(ncol(mf), 5)

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(num_doses(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 0)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(is_randomising(x), TRUE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(tox_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_equal(eff_at_dose(x), c(0,0,0,0,0))
  expect_true(is.integer(eff_at_dose(x)))
  expect_equal(length(eff_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(empiric_eff_rate(x)))
  expect_equal(length(empiric_eff_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_eff(x)))
  expect_equal(length(mean_prob_eff(x)), num_doses(x))

  expect_true(is.numeric(median_prob_eff(x)))
  expect_equal(length(median_prob_eff(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.numeric(prob_eff_quantile(x, p = 0.9)))
  expect_equal(length(prob_eff_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_eff_exceeds(x, 0.5)))
  expect_equal(length(prob_eff_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_true(is.data.frame(prob_tox_samples(x)))
  expect_true(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  expect_true(is.data.frame(prob_eff_samples(x)))
  expect_true(is.data.frame(prob_eff_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))



  # Example 3, using tibble of outcomes
  outcomes <- tibble(
    cohort = c(1,1,1, 2,2,2),
    dose = c(1,1,1, 2,2,2),
    tox = c(0,0, 0,0, 1,1),
    eff = c(0,1, 0,0, 1,0)
  )
  x <- fit(model_fitter, outcomes)

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.33)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.05)
  expect_true(is.numeric(eff_limit(x)))

  expect_equal(num_patients(x), 6)
  expect_true(is.integer(num_patients(x)))

  expect_equal(cohort(x), c(1,1,1, 2,2,2))
  expect_true(is.integer(cohort(x)))
  expect_equal(length(cohort(x)), num_patients(x))

  expect_equal(doses_given(x), unname(c(1,1,1, 2,2,2)))
  expect_true(is.integer(doses_given(x)))
  expect_equal(length(doses_given(x)), num_patients(x))

  expect_equal(tox(x), c(0,0,0, 0,1,1))
  expect_true(is.integer(tox(x)))
  expect_equal(length(tox(x)), num_patients(x))

  expect_true(is.numeric(weight(x)))
  expect_equal(length(weight(x)), num_patients(x))

  expect_equal(num_tox(x), 2)
  expect_true(is.integer(num_tox(x)))

  expect_equal(eff(x), c(0,1,0, 0,1,0))
  expect_true(is.integer(eff(x)))
  expect_equal(length(eff(x)), num_patients(x))

  expect_equal(num_eff(x), 2)
  expect_true(is.integer(num_eff(x)))

  expect_true(all((model_frame(x) - data.frame(patient = c(1,2,3,4,5,6),
                                               cohort = c(1,1,1,2,2,2),
                                               dose = c(1,1,1,2,2,2),
                                               tox = c(0,0,0,0,1,1),
                                               eff = c(0,1,0,0,1,0))) == 0))
  expect_equal(nrow(model_frame(x)), num_patients(x))

  expect_equal(num_doses(x), 5)
  expect_true(is.integer(tox(x)))

  expect_equal(dose_indices(x), 1:5)
  expect_true(is.integer(dose_indices(x)))
  expect_equal(length(dose_indices(x)), num_doses(x))

  expect_true(is.integer(recommended_dose(x)))
  expect_equal(length(recommended_dose(x)), 1)

  expect_equal(continue(x), TRUE)
  expect_true(is.logical(continue(x)))

  expect_equal(n_at_dose(x), c(3,3,0,0,0))
  expect_true(is.integer(n_at_dose(x)))
  expect_equal(length(n_at_dose(x)), num_doses(x))

  expect_equal(n_at_dose(x, dose = 0), 0)
  expect_true(is.integer(n_at_dose(x, dose = 0)))
  expect_equal(length(n_at_dose(x, dose = 0)), 1)

  expect_equal(n_at_dose(x, dose = 1), 3)
  expect_true(is.integer(n_at_dose(x, dose = 1)))
  expect_equal(length(n_at_dose(x, dose = 1)), 1)

  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), TRUE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_true(is.numeric(prob_administer(x)))
  expect_equal(length(prob_administer(x)), num_doses(x))

  expect_equal(tox_at_dose(x), c(0,2,0,0,0))
  expect_true(is.integer(tox_at_dose(x)))
  expect_equal(length(tox_at_dose(x)), num_doses(x))

  expect_equal(eff_at_dose(x), c(1,1,0,0,0))
  expect_true(is.integer(eff_at_dose(x)))
  expect_equal(length(eff_at_dose(x)), num_doses(x))

  expect_true(is.numeric(empiric_tox_rate(x)))
  expect_equal(length(empiric_tox_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_tox(x)))
  expect_equal(length(mean_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(median_prob_tox(x)))
  expect_equal(length(median_prob_tox(x)), num_doses(x))

  expect_true(is.numeric(empiric_eff_rate(x)))
  expect_equal(length(empiric_eff_rate(x)), num_doses(x))

  expect_true(is.numeric(mean_prob_eff(x)))
  expect_equal(length(mean_prob_eff(x)), num_doses(x))

  expect_true(is.numeric(median_prob_eff(x)))
  expect_equal(length(median_prob_eff(x)), num_doses(x))

  expect_true(is.logical(dose_admissible(x)))
  expect_equal(length(dose_admissible(x)), num_doses(x))

  expect_true(is.numeric(prob_tox_quantile(x, p = 0.9)))
  expect_equal(length(prob_tox_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_tox_exceeds(x, 0.5)))
  expect_equal(length(prob_tox_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.numeric(prob_eff_quantile(x, p = 0.9)))
  expect_equal(length(prob_eff_quantile(x, p = 0.9)), num_doses(x))

  expect_true(is.numeric(prob_eff_exceeds(x, 0.5)))
  expect_equal(length(prob_eff_exceeds(x, 0.5)), num_doses(x))

  expect_true(is.logical(supports_sampling(x)))

  expect_true(is.data.frame(prob_tox_samples(x)))
  expect_true(is.data.frame(prob_tox_samples(x, tall = TRUE)))

  expect_true(is.data.frame(prob_eff_samples(x)))
  expect_true(is.data.frame(prob_eff_samples(x, tall = TRUE)))

  # Expect summary to not error. This is how that is tested, apparently:
  expect_error(summary(x), NA)
  expect_output(print(x))
  expect_true(tibble::is_tibble(as_tibble(x)))
  expect_true(nrow(as_tibble(x)) >= num_doses(x))

})
