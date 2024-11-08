
patient_arrivals_func <- function(current_data) cohorts_of_n(n = 1)
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
design <- get_dfcrm_tite(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12)
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
set.seed(2024)
sims <- design %>%
  simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
                  sample_patient_arrivals = patient_arrivals_func,
                  max_time = 10, return_all_fits = TRUE)

# Check out the first simulated trial:
simulated_trial1 <- sims$fits[[1]]

# Look at sequential fits:
simulated_trial1[[1]]$fit # Trial start. Nothing happened yet.
simulated_trial1[[1]]$time # 0, obvs
simulated_trial1[[2]]$fit # First decision
simulated_trial1[[2]]$time # Time of arrival of 1st patient. No follow-up yet!
simulated_trial1[[3]]$fit  # Second decision, i.e. first "proper" decision
simulated_trial1[[3]]$time # Time of second decision
simulated_trial1[[4]]$fit # Third decision
simulated_trial1[[4]]$time # Time of third decision
# Notice weight column is ticking up, and times are getting later
