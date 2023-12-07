
# Using TPI ----
design <- get_tpi(num_doses = 8, target = 0.25,
                  k1 = 1, k2 = 1.5,
                  exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30) %>%
  select_tpi_mtd(exclusion_certainty = 0.95)
sc1 <- simulate_trials(
  design,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)
sc1 # Way off

design_ <- get_tpi(num_doses = 8, target = 0.25,
                   k1 = 1, k2 = 1.5,
                   exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)
sc1_ <- simulate_trials(
  design_,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)
sc1_ # Closer than above but still off

# Using mTPI ----
design <- get_mtpi(num_doses = 8, target = 0.25,
                   epsilon1 = 0.05, epsilon2 = 0.05,
                   exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30) %>%
  select_mtpi_mtd(exclusion_certainty = 0.95)
sc1 <- simulate_trials(
  design,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)
sc1 # Way off

design_ <- get_mtpi(num_doses = 8, target = 0.25,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)
sc1_ <- simulate_trials(
  design_,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)
sc1_ # Closer than above but still off


# Using mTPI2 ----
design <- get_mtpi2(num_doses = 8, target = 0.25,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30) %>%
  select_mtpi2_mtd(exclusion_certainty = 0.95)
sc1 <- simulate_trials(
  design,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)
sc1 # Close

design_ <- get_mtpi2(num_doses = 8, target = 0.25,
                     epsilon1 = 0.05, epsilon2 = 0.05,
                     exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)
sc1_ <- simulate_trials(
  design_,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)
sc1_ # Way off


test_that("Can reproduce AZ mTPI2 app", {

  design <- get_mtpi2(num_doses = 8, target = 0.25,
                      epsilon1 = 0.05, epsilon2 = 0.05,
                      exclusion_certainty = 0.95) %>%
    stop_at_n(n = 30) %>%
    select_mtpi2_mtd(exclusion_certainty = 0.95)

  # No DLTs
  design %>% fit("1N") %>% recommended_dose()

  design %>% fit("2N") %>% recommended_dose()

  # One DLTs
  design %>% fit("1T") %>% recommended_dose()
  design %>% fit("1NT") %>% recommended_dose()
  design %>% fit("1NNT") %>% recommended_dose()
  design %>% fit("1NNNT") %>% recommended_dose()
  design %>% fit("1NNNNT") %>% recommended_dose()
  design %>% fit("1NNNNNT") %>% recommended_dose()

  design %>% fit("2T") %>% recommended_dose()
  design %>% fit("2NT") %>% recommended_dose()
  design %>% fit("2NNT") %>% recommended_dose()
  design %>% fit("2NNNT") %>% recommended_dose()
  design %>% fit("2NNNNT") %>% recommended_dose()
  design %>% fit("2NNNNNT") %>% recommended_dose()

  # Two DLTs
  design %>% fit("1TT") %>% recommended_dose()
  design %>% fit("1NTT") %>% recommended_dose()
  design %>% fit("1NNTT") %>% recommended_dose()
  design %>% fit("1NNNTT") %>% recommended_dose()
  design %>% fit("1NNNNTT") %>% recommended_dose()
  design %>% fit("1NNNNNTT") %>% recommended_dose()
  design %>% fit("1NNNNNNTT") %>% recommended_dose()
  design %>% fit("1NNNNNNNTT") %>% recommended_dose()
  design %>% fit("1NNNNNNNNTT") %>% recommended_dose()
  design %>% fit("1NNNNNNNNNTT") %>% recommended_dose()

  design %>% fit("2TT") %>% recommended_dose()
  design %>% fit("2NTT") %>% recommended_dose()
  design %>% fit("2NNTT") %>% recommended_dose()
  design %>% fit("2NNNTT") %>% recommended_dose()
  design %>% fit("2NNNNTT") %>% recommended_dose()
  design %>% fit("2NNNNNTT") %>% recommended_dose()
  design %>% fit("2NNNNNNTT") %>% recommended_dose()
  design %>% fit("2NNNNNNNTT") %>% recommended_dose()
  design %>% fit("2NNNNNNNNTT") %>% recommended_dose()
  design %>% fit("2NNNNNNNNNTT") %>% recommended_dose()

})
