
#' @importFrom stats rbinom
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom dplyr tibble
phase1_tite_sim <- function(
  selector_factory,
  true_prob_tox,
  patient_sample = PatientSample$new(),
  sample_patient_arrivals = function(df) cohorts_of_n(n=1, mean_time_delta=1),
  previous_outcomes = "",
  # next_dose = NULL,
  time_now = NULL,
  max_time,
  min_fup_time = 0,
  get_weight = linear_follow_up_weight,
  i_like_big_trials = FALSE, # Safety mechanism to avoid infinite trials
  return_all_fits = FALSE
) {

  if(length(max_time) > 1 | max_time <= 0) {
    stop("max_time should be a strictly positive scalar.")
  }

  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(previous_outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  } else {
    base_df <- parse_phase1_outcomes('', as_list = FALSE)
  }
  all_data <- base_df
  if(nrow(base_df) > 0) {
    # message(
    #   "Previous outcomes provided. Setting weight for those patients to 1."
    # )
    base_df$weight <- 1
    if(is.null(time_now)) {
      if("time" %in% colnames(base_df)) {
        time_now <- max(base_df$time)
      }
    }
  }
  if(is.null(time_now)) {
    time_now <- 0
  }

  cohort <- base_df$cohort
  next_cohort <- ifelse(length(cohort) > 0, max(cohort) + 1, 1)
  i <- 1 # dose-decision counter
  max_i <- 30 # Maximum number of dose decisions to make; ignored if
              # i_like_big_trials = TRUE.
  fit <- selector_factory %>% fit(base_df)
  next_dose <- recommended_dose(fit)
  fits <- list()
  fits[[1]] <- list(.depth = i, time = time_now, fit = fit)

  # Vectors for variables for patients in TITE phase
  tite_dose <- integer(length = 0)
  tite_cohort <- integer(length = 0)
  tite_time <- numeric(length = 0)
  while( fit %>% continue() & !is.na(next_dose) &
         (i_like_big_trials | i < max_i) ) {

    new_pts <- sample_patient_arrivals(all_data)
    arrival_time_deltas <- cumsum(new_pts$time_delta)
    n_new_pts <- nrow(new_pts)
    new_dose <- rep(next_dose, n_new_pts)
    new_cohort <- rep(next_cohort, n_new_pts)

    # TITE patient data
    tite_cohort <- c(tite_cohort, new_cohort)
    tite_dose <- c(tite_dose, new_dose)
    tite_time <- c(tite_time, time_now + arrival_time_deltas)
    tite_data <- tibble(
      cohort = tite_cohort,
      dose = tite_dose,
      time = tite_time
    )

    # Decision point
    time_now <- time_now + sum(new_pts$time_delta) + min_fup_time
    if(nrow(tite_data) > 0) {
      tite_data$tox <- patient_sample$get_patient_tox(
        i = seq_along(tite_dose),
        prob_tox = true_prob_tox[tite_dose],
        time = time_now - tite_time
      )
    } else {
      tite_data$tox <- integer(length = 0)
    }
    tite_data$weight <- get_weight(now_time = time_now,
                                   recruited_time = tite_data$time,
                                   tox = tite_data$tox,
                                   max_time = max_time)
    all_data <- bind_rows(base_df, tite_data)
    all_data$patient <- seq_len(nrow(all_data))
    fit <- selector_factory %>% fit(all_data)
    next_dose <- recommended_dose(fit)
    i <- i + 1
    next_cohort <- next_cohort + 1
    fits[[i]] <- list(.depth = i, time = time_now, fit = fit)
  }

  # Warn about i_like_big_trials if sim stopped because of too big i.
  if(!i_like_big_trials & i >= max_i) {
    warning(paste(
      "Simulation stopped because max depth reached.",
      "Set 'i_like_big_trials = TRUE' to avoid this constraint. "))
  }

  # Get final dose recommendation at fullest follow-up.
  # But don't override a scenario where no-dose has been advocated.
  if(!is.na(next_dose)) {
    time_now <- time_now + max_time - min_fup_time
    tite_data <- tibble(
      cohort = tite_cohort,
      dose = tite_dose,
      time = tite_time
    )
    if(nrow(tite_data) > 0) {
      tite_data$tox <- patient_sample$get_patient_tox(
        i = seq_along(tite_dose),
        prob_tox = true_prob_tox[tite_dose],
        time = time_now - tite_time
      )
    } else {
      tite_data$tox <- integer(length = 0)
    }
    tite_data$weight <- get_weight(now_time = time_now,
                                   recruited_time = tite_data$time,
                                   tox = tite_data$tox,
                                   max_time = max_time)
    all_data <- bind_rows(base_df, tite_data)
    all_data$patient <- seq_len(nrow(all_data))
    fit <- selector_factory %>% fit(all_data)
    i <- i + 1
    fits[[i]] <- list(.depth = i, time = time_now, fit = fit)
  }

  if(return_all_fits) {
    return(fits)
  } else {
    return(tail(fits, 1))
  }
}
