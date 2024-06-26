
test_that('trialr_efftox_selector supports correct interface.', {

  efftox_priors <- trialr::efftox_priors
  p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                     beta_mean = 1.5482, beta_sd = 3.5018,
                     gamma_mean = 0.7367, gamma_sd = 2.5423,
                     zeta_mean = 3.4181, zeta_sd = 2.4406,
                     eta_mean = 0, eta_sd = 0.2,
                     psi_mean = 0, psi_sd = 1)
  real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
  model_fitter <- get_trialr_efftox(real_doses = real_doses,
                                    efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                                    p_e = 0.1, p_t = 0.1,
                                    eff0 = 0.5, tox1 = 0.65,
                                    eff_star = 0.7, tox_star = 0.25,
                                    priors = p, seed = 2020)

  # Example 1, using outcome string
  x <- fit(model_fitter, '1NEN 2NBT')
  check_dose_selector_consistency(x)

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.3)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.5)
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

  expect_equal(recommended_dose(x), 3)
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

  expect_equal(n_at_dose(x, dose = 'recommended'), 0)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 0)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
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
  check_dose_selector_consistency(x)

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.3)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.5)
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

  expect_equal(recommended_dose(x), 1)
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

  expect_equal(n_at_dose(x, dose = 'recommended'), 0)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 0)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
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
  check_dose_selector_consistency(x)

  expect_true(is.null(tox_target(x)))

  expect_equal(tox_limit(x), 0.3)
  expect_true(is.numeric(tox_limit(x)))

  expect_equal(eff_limit(x), 0.5)
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

  expect_equal(recommended_dose(x), 3)
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

  expect_equal(n_at_dose(x, dose = 'recommended'), 0)
  expect_true(is.integer(n_at_dose(x, dose = 'recommended')))
  expect_equal(length(n_at_dose(x, dose = 'recommended')), 1)

  expect_equal(n_at_recommended_dose(x), 0)
  expect_true(is.integer(n_at_recommended_dose(x)))
  expect_equal(length(n_at_recommended_dose(x)), 1)

  expect_equal(is_randomising(x), FALSE)
  expect_true(is.logical(is_randomising(x)))
  expect_equal(length(is_randomising(x)), 1)

  expect_equal(unname(prob_administer(x)), c(0.5,0.5,0,0,0))
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
