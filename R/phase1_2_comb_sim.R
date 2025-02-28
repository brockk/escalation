
#' @importFrom stats rbinom
#' @importFrom magrittr %>%
#' @importFrom utils tail
phase1_2_comb_sim <- function(
  selector_factory,
  true_prob_tox,
  true_prob_eff = NULL,
  patient_sample = PatientSample$new(),
  sample_patient_arrivals = function(df) cohorts_of_n(n=3, mean_time_delta=1),
  previous_outcomes = '',
  next_dose = NULL,
  i_like_big_trials = FALSE, # Safety mechanism to avoid infinite trials
  return_all_fits = FALSE
) {

  if(!is.matrix(true_prob_tox)) {
    stop("true_prob_tox should be a matrix")
  }
  if(!is.null(true_prob_eff)) {
    if(!is.matrix(true_prob_eff)) {
      stop("true_prob_eff should be a matrix")
    }
  }

  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_2_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(previous_outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  } else{
    base_df <- parse_phase1_outcomes('', as_list = FALSE)
  }
  dose <- base_df$dose
  dose_s <- base_df$dose_string
  tox <- base_df$tox
  if(is.null(true_prob_eff)) {
    eff <- NULL
  } else {
    eff <- base_df$eff
  }
  cohort <- base_df$cohort
  next_cohort <- ifelse(length(cohort) > 0, max(cohort) + 1, 1)
  if('time' %in% colnames(base_df)) {
    time <- base_df$time
  } else {
    time <- rep(0, length(dose))
  }

  i <- 1 # dose-decision counter
  max_i <- 30 # Maximum number of dose decisions to make; ignored if
              # i_like_big_trials = TRUE.
  time_now <- 0
  fit <- selector_factory %>% fit(base_df)
  if(is.null(next_dose)) next_dose <- fit %>% recommended_dose()
  fits <- list()
  fits[[1]] <- list(.depth = i, time = time_now, fit = fit)
  while(fit %>% continue() & !any(is.na(next_dose)) &
        (i_like_big_trials | i < max_i)) {

    current_data <- data.frame(
      cohort = cohort,
      patient = seq_along(dose_s),
      # dose = dose,
      dose_string = dose_s,
      tox = tox,
      # eff = eff,
      time = time
    )
    if(!is.null(true_prob_eff)) {
      current_data$eff <- eff
    }
    new_pts <- sample_patient_arrivals(current_data)
    arrival_time_deltas <- cumsum(new_pts$time_delta)
    n_new_pts <- nrow(new_pts)
    next_dose_s <- dose_vector_to_string(next_dose)
    new_dose_s <- rep(next_dose_s, n_new_pts)
    new_pt_indices <- nrow(current_data) + seq(1, n_new_pts)
    new_tox <- patient_sample$get_patient_tox(
      i = new_pt_indices,
      prob_tox = true_prob_tox[t(cbind(next_dose))]
    )
    if(is.null(true_prob_eff)) {
      new_eff <- NULL
    } else {
      new_eff <- patient_sample$get_patient_eff(
        i = new_pt_indices,
        prob_eff = true_prob_eff[t(cbind(next_dose))]
      )
    }
    new_cohort <- rep(next_cohort, n_new_pts)

    dose_s <- c(dose_s, new_dose_s)
    tox <- c(tox, new_tox)
    if(!is.null(true_prob_eff)) {
      eff <- c(eff, new_eff)
    }
    cohort <- c(cohort, new_cohort)
    time <- c(time, time_now + arrival_time_deltas)

    z <- c(
      length(cohort),
      length(dose_s),
      length(tox),
      length(time)
    )
    if(!is.null(true_prob_eff)) {
      z <- c(z, length(eff))
    }
    if(min(z) != max(z)) {
      stop("Unexpected vector lengths. This looks like a bug.")
    }

    new_data = data.frame(
      cohort = cohort,
      patient = seq_along(dose_s),
      # dose = dose,
      dose_string = dose_s,
      tox = tox,
      # eff = eff,
      time = time
    )
    new_data$dose <- map(new_data$dose_string, dose_string_to_vector)
    if(!is.null(true_prob_eff)) {
      new_data$eff <- eff
    }

    time_now <- time_now + max(arrival_time_deltas)
    i <- i + 1
    fit <- selector_factory %>% fit(new_data)
    next_cohort <- next_cohort + 1
    fits[[i]] <- list(.depth = i, time = time_now, fit = fit)
    next_dose <- fit %>% recommended_dose()
  }

  # Warn about i_like_big_trials if sim stopped because of too big i.
  if(!i_like_big_trials & i >= max_i) {
    warning(paste(
      "Simulation stopped because max depth reached.",
      "Set 'i_like_big_trials = TRUE' to avoid this constraint. "))
  }

  if(return_all_fits) {
    return(fits)
  } else {
    return(tail(fits, 1))
  }
}
