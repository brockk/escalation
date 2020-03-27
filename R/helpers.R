
# Make sure columns in an outcomes data-frame are integers.
spruce_outcomes_df <- function(df) {
  df$dose <- as.integer(df$dose)
  df$tox <- as.integer(df$tox)
  if('cohort' %in% colnames(df)) df$cohort <- as.integer(df$cohort)
  if('patient' %in% colnames(df)) df$patient <- as.integer(df$patient)
  df
}


#' @importFrom tibble tibble
model_frame_to_counts <- function(model_frame, num_doses) {

  if(num_doses <= 0) {
    df_c <- tibble(dose = integer(length = 0), n = integer(length = 0),
                  tox = integer(length = 0))
  } else {
    df <- model_frame
    dose_indices <- 1:num_doses
    dose_counts <- map_int(dose_indices, ~ sum(df$dose == .x))
    tox_counts <- map_int(dose_indices, ~ sum(df$tox[df$dose == .x]))
    df_c <- tibble(dose = dose_indices, n = dose_counts, tox = tox_counts)
  }

  if('eff' %in% colnames(df)) {
    df_c$eff <- map_int(dose_indices, ~ sum(df$eff[df$dose == .x]))
  }

  df_c
}

# Copied from BOIN package (https://cran.r-project.org/package=BOIN) to
# perform isotonic regression in order to work with their method.
boin_pava <- function(x, wt = rep(1, length(x))) {
  n <- length(x)
  if (n <= 1)
    return(x)
  if (any(is.na(x)) || any(is.na(wt))) {
    stop("Missing values in 'x' or 'wt' not allowed")
  }
  lvlsets <- (1:n)
  repeat {
    viol <- (as.vector(diff(x)) < 0)
    if (!(any(viol)))
      break
    i <- min((1:(n - 1))[viol])
    lvl1 <- lvlsets[i]
    lvl2 <- lvlsets[i + 1]
    ilvl <- (lvlsets == lvl1 | lvlsets == lvl2)
    x[ilvl] <- sum(x[ilvl] * wt[ilvl])/sum(wt[ilvl])
    lvlsets[ilvl] <- lvl1
  }
  x
}

#' @importFrom gtools inv.logit
#' @importFrom stats rnorm
#' @importFrom tidyr as_tibble
#' @importFrom dplyr mutate select
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
get_posterior_prob_tox_sample <- function(dfcrm_selector, iter) {
  selector <- dfcrm_selector

  if(num_patients(selector) > 0) {
    if(selector$dfcrm_fit$model == 'empiric') {
      # Sample beta from normal distribution with mean and stdev that match
      # posterior parameter estimates:
      beta <- rnorm(n = iter, mean = selector$dfcrm_fit$estimate,
                    sd = sqrt(selector$dfcrm_fit$post.var))
      # Matrix with skeleton in each row
      skeleton_matrix <- matrix(selector$skeleton, nrow = iter,
                                ncol = num_doses(selector), byrow = TRUE)
      # Raise each row to one of the sampled beta values:
      prob_tox_sample <- skeleton_matrix ^ exp(beta)
      # prob_tox_sample
    } else if(selector$dfcrm_fit$model == 'logistic') {
      # dfcrm fixes the intercept value:
      alpha <- selector$dfcrm_fit$intcpt
      # Sample beta from normal distribution with mean and stdev that match
      # posterior parameter estimates:
      beta <- rnorm(n = iter, mean = selector$dfcrm_fit$estimate,
                    sd = sqrt(selector$dfcrm_fit$post.var))
      # Matrix with scaled dose in each row
      dose_matrix <- matrix(selector$dfcrm_fit$dosescaled, nrow = iter,
                            ncol = num_doses(selector), byrow = TRUE)
      # Perform the transform described in dfcrm manual:
      prob_tox_sample <- inv.logit((alpha + exp(beta) * dose_matrix))
      # prob_tox_sample
    } else {
      stop(paste0("Don't know what to do with dfcrm model '",
                  selector$dfcrm_fit$model, "'"))
    }
  } else {
    prob_tox_sample <- matrix(ncol = num_doses(selector), nrow = 0)
  }

  . <- .draw <- NULL
  abc <- as.data.frame(prob_tox_sample)
  colnames(abc) <- as.character(dose_indices(selector))
  abc <- abc %>%
    mutate(.draw = row.names(.)) %>%
    select(.draw, everything())
  as_tibble(abc)
}

#' Sample times between patient arrivals using the exponential distribution.
#'
#' @param n integer, sample arrival times for this many patients.
#' @param mean_time_delta the average gap between patient arrival times. I.e.
#' the reciprocal of the rate parameter in an Exponential distribution.
#' @return \code{data.frame} with column time_delta containing durations of time
#' between patient arrivals.
#'
#' @export
#'
#' @importFrom stats rexp
#' @examples
#' cohorts_of_n()
#' cohorts_of_n(n = 10, mean_time_delta = 5)
cohorts_of_n <- function(n = 3, mean_time_delta = 1) {
  time_delta <- rexp(n = n, rate = 1 / mean_time_delta) %>% round(1)
  data.frame(time_delta = time_delta)
}
