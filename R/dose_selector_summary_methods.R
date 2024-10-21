
.dose_selector_summary <- function(x, ...) {
  .dose_selector_to_tibble(x, ...)
}

#' @importFrom tibble tibble
.dose_selector_to_tibble <- function(x, ...) {
  d_s <- dose_strings(x)
  dose_labs <- c('NoDose', d_s)
  rec_d <- recommended_dose(x)
  if(length(rec_d) == 1) {
    if(is.na(rec_d)) {
      rec_bool <- c(TRUE, rep(FALSE, length(d_s)))
    } else {
      rec_bool <- c(FALSE, dose_indices(x) == rec_d)
    }
  } else {
    rec_d_str <- dose_vector_to_string(rec_d)
    if(is.na(rec_d_str)) {
      rec_bool <- c(TRUE, rep(FALSE, length(d_s)))
    } else {
      rec_bool <- c(FALSE, d_s == rec_d_str)
    }
  }

  tb <- tibble(
    dose = ordered(dose_labs, levels = dose_labs),
    tox = c(0, tox_at_dose(x)),
    n = c(0, n_at_dose(x)),
    empiric_tox_rate = c(0, empiric_tox_rate(x)),
    mean_prob_tox = c(0, mean_prob_tox(x)),
    median_prob_tox = c(0, median_prob_tox(x)),
    admissible = c(TRUE, dose_admissible(x)),
    recommended = rec_bool
  )
  if(is_randomising(x)) {
    tb$prob_rand = c(0, prob_administer(x))
  }
  tb
}

.dose_selector_print <- function(x, ...) {
  # Patient-level data
  if(num_patients(x) > 0) {
    cat('Patient-level data:\n')
    df <- model_frame(x)
    colnames(df) <- str_to_title(colnames(df))
    print(df)
  } else {
    cat('No patients have been treated.\n')
  }
  cat('\n')

  # Dose-level data
  if(num_doses(x) > 0) {
    cat('Dose-level data:\n')
    df <- summary(x)
    print(df, digits = 3)
  } else {
    cat('No doses are under investigation.\n')
  }
  cat('\n')

  # Toxicity target
  tt <- tox_target(x)
  if(!is.null(tt)) {
    if(!is.na(tt)) {
      cat(paste0('The model targets a toxicity level of ', tt, '.'))
      cat('\n')
    }
  }

  # Dose recommendation and continuance
  recd <- recommended_dose(x)
  cont <- continue(x)
  if(is.na(recd)) {
    if(cont) {
      cat(paste0('The model advocates continuing but recommends no dose.'))
    } else {
      cat(paste0('The model advocates stopping and recommending no dose.'))
    }
  } else {
    if(cont) {
      cat(paste0('The model advocates continuing at dose ', recd, '.'))
    } else {
      cat(paste0('The model advocates stopping and recommending dose ', recd,
                 '.'))
    }
  }
  cat('\n')
}

#' @importFrom purrr map_int
.n_at_dose <- function(x, dose = NULL, ...) {
  if(is.null(dose)) {
    di <- dose_indices(x)
    return(map_int(di, ~ sum(doses_given(x) == .x)))
  } else if(is.na(dose)) {
    return(NA)
  } else if(dose == 'recommended') {
    return(n_at_recommended_dose(x))
  } else {
    return(sum(doses_given(x) == dose))
  }
}

.n_at_recommended_dose <- function(x, ...) {
  return(n_at_dose(x, dose = recommended_dose(x), ...))
}
