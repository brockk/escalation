
# Helpers.R
cohorts_of_n <- function(n = 3, mean_time_delta = 1) {
  time_delta <- rexp(n = n, rate = 1 / mean_time_delta) %>% round(1)
  data.frame(time_delta = time_delta)
}
# cohorts_of_n()

# selector_factory.R? interface.R?
sim1 <- function(
  selector_factory,
  true_prob_tox,
  sample_patient_arrivals,
  previous_outcomes,
  next_dose,
  i_like_big_trials) {
  UseMethod('sim1')
}


# ?
#' @export
sim1.derived_dose_selector_factory <- function(selector_factory, ...) {
  return(selector_factory$parent %>% sim1(...))
}

# ?
# selector_factory <- crm_fitter
# sample_patient_arrivals = function() cohorts_of_n(n = 3, mean_time_delta = 1)
# previous_outcomes = ''
# next_dose = NULL
# i_like_big_trials = FALSE

sim1.tox_selector_factory <- function(
  selector_factory,
  true_prob_tox,
  sample_patient_arrivals = function() cohorts_of_n(n = 3, mean_time_delta = 1),
  previous_outcomes = '',
  next_dose = NULL,
  i_like_big_trials = FALSE # Safety net if stop_trial_func is misspecified...
) {

  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  }

  i <- 0 # loop counter
  max_i <- 30

  fit <- selector_factory %>% fit(previous_outcomes)
  if(is.null(next_dose)) next_dose <- fit %>% recommended_dose()
  while(fit %>% continue() & (i_like_big_trials | i < max_i)) {

    # next_dose <-

    i <- i + 1
  }

  print(sample_patient_arrivals)
}



# simulate_efftox_trialr <- function(
#   num_sims, true_tox, cohort_sizes, first_dose, ...) {
#
#   recommended_dose <- integer(length = num_sims)
#   efficacies <- list()
#   toxicities <- list()
#   doses_given <- list()
#
#   for(i in 1:num_sims) {
#     print(paste('Starting iteration', i))
#     this_dat <- dat
#     dose <- first_dose
#     for(cohort_size in cohort_sizes) {
#       prob_eff <- true_eff[dose]
#       prob_tox <- true_tox[dose]
#       # Simulate new efficacy events
#       new_eff <- stats::rbinom(n = cohort_size, size = 1, prob = prob_eff)
#       new_tox <- stats::rbinom(n = cohort_size, size = 1, prob = prob_tox)
#       # And append to trial data
#       this_dat$eff <- c(this_dat$eff, new_eff)
#       this_dat$tox <- c(this_dat$tox, new_tox)
#       # Also reflect doses delivered
#       this_dat$doses <- c(this_dat$doses, rep(dose, cohort_size))
#       this_dat$num_patients <- this_dat$num_patients + cohort_size
#       samp <- rstan::sampling(stanmodels$EffTox, data = this_dat, ...)
#       l <- efftox_process(this_dat, samp)
#       # Select a dose?
#       if(sum(l$acceptable) > 0) {
#         # Select dose
#         dose = which.max(ifelse(l$acceptable, l$utility, NA))
#       } else {
#         dose <- NA
#         break()
#       }
#     }
#     recommended_dose[i] = dose
#     efficacies[[i]] = this_dat$eff
#     toxicities[[i]] = this_dat$tox
#     doses_given[[i]] = this_dat$doses
#   }
#
#   return(list(recommended_dose = recommended_dose,
#               efficacies = efficacies,
#               toxicities = toxicities,
#               doses_given = doses_given))
# }
#
# # next_cohort_size_func: int function(crm_fit)
# # select_dose_func: int function(crm_fit)
# # stop_trial_func: bool function(crm_fit)
# # next_dose: int
# simulate_crm_trial <- function(
#   true_prob_dlt,
#   next_cohort_size_func = function(crm_fit) { 3 },
#   select_dose_func = function(crm_fit) {
#     crm_fit$recommended_dose
#   },
#   stop_trial_func = function(crm_fit) { FALSE },
#   next_dose = NULL,
#   i_like_big_trials = FALSE, # Safety net if stop_trial_func is misspecified
#   current_trial_state = NULL,
#   ...
# ) {
#
#     # Stopping the trial should be governed by stop_trial_func().
#     # However, coding errors happen and never-evending processes
#     # are unwelcome. So unless i_like_big_trials = TRUE, the
#     # trial simulation will stop arbitratily at:
#     N <- 30
#     # Why this value?
#     # It is divisible by 3 and about the size of most CRM trials.
#
#     # Init iteration
#     if(is.null(current_trial_state)) {
#       doses <- tox <- c()
#       fit <- stan_crm(outcome_str = '', ...)
#     } else {
#       doses <- current_trial_state$doses
#       tox <- current_trial_state$tox
#       fit <- current_trial_state
#     }
#     # If next-dose is provided, use it. Else derive it.
#     if(is.null(next_dose))
#       d <- fit$recommended_dose
#     else
#       d <- next_dose
#
#     this_path <- list()
#
#     continue <- !stop_trial_func(fit)
#     node_id <- 1
#     parent_node_id <- NA
#     depth <- 0
#     these_outcomes <- to_outcome_str(fit)
#
#     node <- dose_finding_path_node(node_id = node_id,
#                                    parent_node_id = parent_node_id,
#                                    depth = depth,
#                                    outcomes = these_outcomes,
#                                    next_dose = d,
#                                    fit = fit,
#                                    parent_fit = NULL)
#     # this_path[[these_outcomes]] <- node
#     this_path[[node_id]] <- node
#
#     while(continue) {
#
#       depth <- depth + 1
#       parent_node_id <- node_id
#       node_id <- node_id + 1
#
#       # The size of the next cohort is flexibly chosen by a delegate
#       cohort_n <- next_cohort_size_func(fit)
#       # cohort_n patients will now be treated at dose d.
#       # Sample tox outcomes for them.
#       these_doses <- rep(d, cohort_n)
#       these_tox <- rbinom(n = cohort_n, size = 1, prob = true_prob_dlt[d])
#       doses <- c(doses, these_doses)
#       tox <- c(tox, these_tox)
#       new_fit <- stan_crm(doses = doses, tox = tox, ...)
#       d <- select_dose_func(new_fit)
#       stop_trial <- stop_trial_func(new_fit) | is.na(d)
#       continue <- !stop_trial
#       these_outcomes <- to_outcome_str(new_fit)
#
#       node <- dose_finding_path_node(node_id = node_id,
#                                      parent_node_id = parent_node_id,
#                                      depth = depth,
#                                      outcomes = these_outcomes,
#                                      next_dose = d,
#                                      fit = new_fit,
#                                      parent_fit = fit)
#       # this_path[[these_outcomes]] <- node
#       this_path[[node_id]] <- node
#
#       # Advance the trial
#       fit <- new_fit
#       # Watch out for runaway trials
#       if(fit$num_patients >= N & !i_like_big_trials)
#         continue <- FALSE
#     }
#
#     class(this_path) <- c("dose_finding_paths", "list")
#     this_path
#   }
