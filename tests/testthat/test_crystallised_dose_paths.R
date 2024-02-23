
test_that('phase I calculate_probabilities does what it should', {

  library(dplyr)

  # Scenario 1 - CRM without stopping ----
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25
  selector_factory <- get_dfcrm(skeleton = skeleton, target = target)

  cohort_sizes <- c(3, 3)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)

  true_prob_tox <- c(0.05, 0.15, 0.4, 0.6, 0.9)
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox)

  expect_is(cdp, 'crystallised_dose_paths')

  df <- as_tibble(cdp$terminal_nodes)
  expect_is(df, 'tbl_df')

  # Outcome probabilities in terminal nodes should sum to 1:
  expect_lt(abs(sum(df$prob_outcomes)) - 1, 0.01)





  # Scenario 2 - 3+3 ----
  selector_factory <- get_three_plus_three(num_doses = 4)
  cohort_sizes <- c(3, 3, 3)

  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)

  true_prob_tox <- c(0.01, 0.02, 0.05, 0.1)
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox)

  expect_is(cdp, 'crystallised_dose_paths')

  df <- as_tibble(cdp$terminal_nodes)
  expect_is(df, 'tbl_df')

  # Outcome probabilities in terminal nodes should sum to 1:
  expect_lt(abs(sum(df$prob_outcomes)) - 1, 0.01)



  # Scenario 3 - BOIN with partial outcomes and manual next-dose ----
  target <- 0.33
  selector_factory <- get_boin(num_doses = 6, target = target)
  cohort_sizes <- c(3, 4, 2)

  paths <- selector_factory %>%
    get_dose_paths(cohort_sizes = cohort_sizes,
                   previous_outcomes = '1NNN', next_dose = 6)

  true_prob_tox <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox)

  expect_is(cdp, 'crystallised_dose_paths')

  df <- as_tibble(cdp$terminal_nodes)
  expect_is(df, 'tbl_df')

  # Outcome probabilities in terminal nodes should sum to 1:
  expect_lt(abs(sum(df$prob_outcomes)) - 1, 0.01)

})

test_that('phase I/II calculate_probabilities does what it should', {

  library(dplyr)

  prob_select = c(0.1, 0.3, 0.5, 0.07, 0.03)
  selector_factory <- get_random_selector(prob_select = prob_select,
                                          supports_efficacy = TRUE)

  true_prob_tox <- c(0.05, 0.15, 0.4, 0.6, 0.9)
  true_prob_eff <- c(0.25, 0.35, 0.5, 0.7, 0.75)


  # Scenario 1 - Random selector in two cohorts of three ----
  cohort_sizes <- c(2, 2)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)

  expect_error(
    cdp <- calculate_probabilities(dose_paths = paths,
                                   true_prob_tox = true_prob_tox)
  )
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox,
                                 true_prob_eff = true_prob_eff)

  expect_is(cdp, 'crystallised_dose_paths')

  df <- as_tibble(cdp$terminal_nodes)
  expect_is(df, 'tbl_df')
  expect_equal(nrow(df), 10^2)
  # Outcome probabilities in terminal nodes should sum to 1:
  expect_lt(abs(sum(df$prob_outcomes)) - 1, 0.01)




  # # Scenario 2 - Three cohorts of two ----
  # cohort_sizes <- c(2, 2, 2)
  #
  # paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
  #
  # expect_error(
  #   cdp <- calculate_probabilities(dose_paths = paths,
  #                                  true_prob_tox = true_prob_tox)
  # )
  # cdp <- calculate_probabilities(dose_paths = paths,
  #                                true_prob_tox = true_prob_tox,
  #                                true_prob_eff = true_prob_eff)
  #
  # expect_is(cdp, 'crystallised_dose_paths')
  #
  # df <- as_tibble(cdp$terminal_nodes)
  # expect_is(df, 'tbl_df')
  # expect_equal(nrow(df), 10^3)
  #
  # # Outcome probabilities in terminal nodes should sum to 1:
  # expect_lt(abs(sum(df$prob_outcomes)) - 1, 0.01)



  # Scenario 3 - Odd-sized cohorts ----
  # cohort_sizes <- c(1, 2, 3)
  cohort_sizes <- c(1, 2)

  paths <- selector_factory %>%
    get_dose_paths(cohort_sizes = cohort_sizes,
                   previous_outcomes = '1NBE', next_dose = 3)

  expect_error(
    cdp <- calculate_probabilities(dose_paths = paths,
                                   true_prob_tox = true_prob_tox)
  )
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox,
                                 true_prob_eff = true_prob_eff)

  expect_is(cdp, 'crystallised_dose_paths')

  df <- as_tibble(cdp$terminal_nodes)
  expect_is(df, 'tbl_df')
  expect_equal(nrow(df), 4 * 10)

  # Outcome probabilities in terminal nodes should sum to 1:
  expect_lt(abs(sum(df$prob_outcomes)) - 1, 0.01)

})

test_that('crystallised_dose_paths supports correct interface.', {

  # Scenario 1 - CRM without stopping ----
  skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
  target <- 0.25
  selector_factory <- get_dfcrm(skeleton = skeleton, target = target)
  cohort_sizes <- c(3, 3)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
  true_prob_tox <- c(0.05, 0.15, 0.4, 0.6, 0.9)
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox)

  expect_lte(num_patients(cdp), 6.01)
  expect_gte(num_patients(cdp), 5.99)
  expect_is(num_patients(cdp), 'numeric')
  expect_equal(length(num_patients(cdp)), 1)

  expect_equal(num_doses(cdp), 5)
  expect_is(num_doses(cdp), 'integer')
  expect_equal(length(num_doses(cdp)), 1)

  expect_equal(dose_indices(cdp), seq(1, 5))
  expect_is(dose_indices(cdp), 'integer')
  expect_equal(length(dose_indices(cdp)), 5)

  expect_lt(continue(cdp), 1.01)
  expect_gt(continue(cdp), 0.99)
  expect_is(continue(cdp), 'numeric')
  expect_equal(length(continue(cdp)), 1)

  expect_true(all(n_at_dose(cdp) >= 0))
  expect_true(is.numeric(n_at_dose(cdp)))
  expect_equal(length(n_at_dose(cdp)), num_doses(cdp))

  expect_true(all(n_at_dose(cdp, dose = 0) >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 0)))
  expect_equal(length(n_at_dose(cdp, dose = 0)), 1)

  expect_true(all(n_at_dose(cdp, dose = 1) >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 1)))
  expect_equal(length(n_at_dose(cdp, dose = 1)), 1)

  expect_true(all(n_at_dose(cdp, dose = 'recommended') >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 'recommended')))
  expect_equal(length(n_at_dose(cdp, dose = 'recommended')), 1)

  expect_true(all(n_at_recommended_dose(cdp) >= 0))
  expect_true(is.numeric(n_at_recommended_dose(cdp)))
  expect_equal(length(n_at_recommended_dose(cdp)), 1)

  expect_true(all(tox_at_dose(cdp) >= 0))
  expect_true(is.numeric(tox_at_dose(cdp)))
  expect_equal(length(tox_at_dose(cdp)), num_doses(cdp))

  expect_true(all(num_tox(cdp) >= 0))
  expect_true(is.numeric(num_tox(cdp)))
  expect_equal(length(num_tox(cdp)), 1)

  expect_true(all(prob_recommend(cdp) >= 0))
  expect_true(all(prob_recommend(cdp) <= 1))
  expect_true(is.numeric(prob_recommend(cdp)))
  expect_equal(length(prob_recommend(cdp)), num_doses(cdp) + 1)

  expect_true(all(prob_administer(cdp) >= 0))
  expect_true(all(prob_administer(cdp) <= 1))
  expect_true(is.numeric(prob_administer(cdp)))
  expect_equal(length(prob_administer(cdp)), num_doses(cdp))


  # Scenario 2 - 3+3 ----
  selector_factory <- get_three_plus_three(num_doses = 4)
  cohort_sizes <- c(3, 3, 3)
  paths <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
  true_prob_tox <- c(0.01, 0.02, 0.05, 0.1)
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox)

  expect_lt(num_patients(cdp), 9)
  expect_gt(num_patients(cdp), 8.8)
  expect_is(num_patients(cdp), 'numeric')
  expect_equal(length(num_patients(cdp)), 1)

  expect_equal(num_doses(cdp), 4)
  expect_is(num_doses(cdp), 'integer')
  expect_equal(length(num_doses(cdp)), 1)

  expect_equal(dose_indices(cdp), seq(1, 4))
  expect_is(dose_indices(cdp), 'integer')
  expect_equal(length(dose_indices(cdp)), 4)

  expect_lt(continue(cdp), 1)
  expect_gt(continue(cdp), 0.9)
  expect_is(continue(cdp), 'numeric')
  expect_equal(length(continue(cdp)), 1)

  expect_true(all(n_at_dose(cdp) >= 0))
  expect_true(is.numeric(n_at_dose(cdp)))
  expect_equal(length(n_at_dose(cdp)), num_doses(cdp))

  expect_true(all(n_at_dose(cdp, dose = 0) >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 0)))
  expect_equal(length(n_at_dose(cdp, dose = 0)), 1)

  expect_true(all(n_at_dose(cdp, dose = 1) >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 1)))
  expect_equal(length(n_at_dose(cdp, dose = 1)), 1)

  expect_true(all(n_at_dose(cdp, dose = 'recommended') >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 'recommended')))
  expect_equal(length(n_at_dose(cdp, dose = 'recommended')), 1)

  expect_true(all(n_at_recommended_dose(cdp) >= 0))
  expect_true(is.numeric(n_at_recommended_dose(cdp)))
  expect_equal(length(n_at_recommended_dose(cdp)), 1)

  expect_true(all(tox_at_dose(cdp) >= 0))
  expect_true(is.numeric(tox_at_dose(cdp)))
  expect_equal(length(tox_at_dose(cdp)), num_doses(cdp))

  expect_true(all(num_tox(cdp) >= 0))
  expect_true(is.numeric(num_tox(cdp)))
  expect_equal(length(num_tox(cdp)), 1)

  expect_true(all(prob_recommend(cdp) >= 0))
  expect_true(all(prob_recommend(cdp) <= 1))
  expect_true(is.numeric(prob_recommend(cdp)))
  expect_equal(length(prob_recommend(cdp)), num_doses(cdp) + 1)

  expect_true(all(prob_administer(cdp) >= 0))
  expect_true(all(prob_administer(cdp) <= 1))
  expect_true(is.numeric(prob_administer(cdp)))
  expect_equal(length(prob_administer(cdp)), num_doses(cdp))


  # Scenario 3 - BOIN ----
  target <- 0.33
  selector_factory <- get_boin(num_doses = 6, target = target)
  cohort_sizes <- c(3, 4, 2)
  paths <- selector_factory %>%
    get_dose_paths(cohort_sizes = cohort_sizes,
                   previous_outcomes = '1NNN', next_dose = 6)
  true_prob_tox <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  cdp <- calculate_probabilities(dose_paths = paths,
                                 true_prob_tox = true_prob_tox)

  expect_lte(num_patients(cdp), 12.01)
  expect_gte(num_patients(cdp), 11.99)
  expect_is(num_patients(cdp), 'numeric')
  expect_equal(length(num_patients(cdp)), 1)

  expect_equal(num_doses(cdp), 6)
  expect_is(num_doses(cdp), 'integer')
  expect_equal(length(num_doses(cdp)), 1)

  expect_equal(dose_indices(cdp), seq(1, 6))
  expect_is(dose_indices(cdp), 'integer')
  expect_equal(length(dose_indices(cdp)), 6)

  expect_lt(continue(cdp), 1.01)
  expect_gt(continue(cdp), 0.99)
  expect_is(continue(cdp), 'numeric')
  expect_equal(length(continue(cdp)), 1)

  expect_true(all(n_at_dose(cdp) >= 0))
  expect_true(is.numeric(n_at_dose(cdp)))
  expect_equal(length(n_at_dose(cdp)), num_doses(cdp))

  expect_true(all(n_at_dose(cdp, dose = 0) >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 0)))
  expect_equal(length(n_at_dose(cdp, dose = 0)), 1)

  expect_true(all(n_at_dose(cdp, dose = 1) >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 1)))
  expect_equal(length(n_at_dose(cdp, dose = 1)), 1)

  expect_true(all(n_at_dose(cdp, dose = 'recommended') >= 0))
  expect_true(is.numeric(n_at_dose(cdp, dose = 'recommended')))
  expect_equal(length(n_at_dose(cdp, dose = 'recommended')), 1)

  expect_true(all(n_at_recommended_dose(cdp) >= 0))
  expect_true(is.numeric(n_at_recommended_dose(cdp)))
  expect_equal(length(n_at_recommended_dose(cdp)), 1)

  expect_true(all(tox_at_dose(cdp) >= 0))
  expect_true(is.numeric(tox_at_dose(cdp)))
  expect_equal(length(tox_at_dose(cdp)), num_doses(cdp))

  expect_true(all(num_tox(cdp) >= 0))
  expect_true(is.numeric(num_tox(cdp)))
  expect_equal(length(num_tox(cdp)), 1)

  expect_true(all(prob_recommend(cdp) >= 0))
  expect_true(all(prob_recommend(cdp) <= 1))
  expect_true(is.numeric(prob_recommend(cdp)))
  expect_equal(length(prob_recommend(cdp)), num_doses(cdp) + 1)

  expect_true(all(prob_administer(cdp) >= 0))
  expect_true(all(prob_administer(cdp) <= 1))
  expect_true(is.numeric(prob_administer(cdp)))
  expect_equal(length(prob_administer(cdp)), num_doses(cdp))

})
