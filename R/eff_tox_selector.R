
#' @export
tox_target.eff_tox_selector <- function(x, ...) {
  # By default:
  return(NULL)
}

#' @export
tox_limit.eff_tox_selector <- function(x, ...) {
  # By default:
  return(NULL)
}

#' @export
eff_limit.eff_tox_selector <- function(x, ...) {
  # By default:
  return(NULL)
}

#' @export
num_eff.eff_tox_selector <- function(x, ...) {
  sum(eff(x))
}

#' @importFrom purrr map_int
#' @export
eff_at_dose.eff_tox_selector <- function(x, ...) {
  dose_indices <- 1:(num_doses(x))
  eff_seen <- eff(x)
  map_int(dose_indices, ~ sum(eff_seen[doses_given(x) == .x]))
}

#' @export
#' @importFrom tibble tibble
model_frame.eff_tox_selector <- function(x, ...) {

  if(num_patients(x) > 0) {
    tibble(
      patient = seq(1, num_patients(x)),
      cohort = cohort(x) %>% as.integer(),
      dose = doses_given(x) %>% as.integer(),
      tox = tox(x) %>% as.integer(),
      eff = eff(x) %>% as.integer()
    )
  } else {
    tibble(
      patient = integer(length = 0),
      cohort = integer(length = 0),
      dose = integer(length = 0),
      tox = integer(length = 0),
      eff = integer(length = 0)
    )
  }
}

#' @export
empiric_eff_rate.eff_tox_selector <- function(x, ...) {
  return(x %>% eff_at_dose() / x %>% n_at_dose())
}

#' @export
#' @importFrom tibble as_tibble
summary.eff_tox_selector <- function(object, ...) {
  as_tibble(object, ...)
}

#' @importFrom stringr str_to_title
#' @importFrom tibble tibble
#' @export
print.eff_tox_selector <- function(x, ...) {

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

  # Toxicity limit
  tl <- tox_limit(x)
  if(!is.null(tl)) {
    if(!is.na(tl)) {
      cat(paste0('The model uses a toxicity limit of ', tl, '.'))
      cat('\n')
    }
  }
  # Efficacy limit
  el <- eff_limit(x)
  if(!is.null(el)) {
    if(!is.na(el)) {
      cat(paste0('The model uses an efficacy limit of ', el, '.'))
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

  # cat(paste0('The dose most likely to be the MTD is ',
  #            x$modal_mtd_candidate, '.'))
  # cat('\n')
  # cat(paste0('Model entropy: ', format(round(x$entropy, 2), nsmall = 2)))
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.eff_tox_selector <- function(x, ...) {

  dose_labs <- c('NoDose', as.character(dose_indices(x)))
  rec_d <- recommended_dose(x)
  if(is.na(rec_d)) {
    rec_bool <- c(TRUE, rep(FALSE, num_doses(x)))
  } else {
    rec_bool <- c(FALSE, dose_indices(x) == rec_d)
  }

  tb <- tibble(
    dose = ordered(dose_labs, levels = dose_labs),
    tox = c(0, tox_at_dose(x)),
    eff = c(0, eff_at_dose(x)),
    n = c(0, n_at_dose(x)),
    empiric_tox_rate = c(0, empiric_tox_rate(x)),
    mean_prob_tox = c(0, mean_prob_tox(x)),
    median_prob_tox = c(0, median_prob_tox(x)),
    empiric_eff_rate = c(0, empiric_eff_rate(x)),
    mean_prob_eff = c(0, mean_prob_eff(x)),
    median_prob_eff = c(0, median_prob_eff(x)),
    admissible = c(TRUE, dose_admissible(x)),
    recommended = rec_bool,
  )
  if(is_randomising(x)) {
    tb$prob_rand = c(0, prob_administer(x))
  }
  tb
}
