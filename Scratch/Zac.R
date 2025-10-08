
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
design <- get_dfcrm_tite(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12)

sample_patient_arrivals = function(df) cohorts_of_n(n = 3, mean_time_delta = 1)
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
set.seed(2024)
sims <- design %>%
  simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
                  sample_patient_arrivals = patient_arrivals_func,
                  max_time = 10, min_fup_time = 5, return_all_fits = TRUE)

# Check out the first simulated trial:
simulated_trial1 <- sims$fits[[1]]

# Look at sequential fits:

simulated_trial1[[1]]$fit # Trial start. Nothing happened yet.
simulated_trial1[[1]]$time # Time of first fit. 0, obvs

simulated_trial1[[2]]$fit # End of first cohort, i.e. three patients.
# Notice last patient has weight 0.5 because I chose length of evaluation window
# is 10 with min FUP = 5, so weight of last patient at analysis point will
# always be 50% when using a linear weight function.
simulated_trial1[[2]]$time # The time 1st cohort ends happens to be 7.01

simulated_trial1[[3]]$fit  # End of second cohort. Notice pat 5 has weight 1
# because they experienced tox! The other cohort 2 patients have partial weight.
simulated_trial1[[3]]$time # Time of second decision, etc
