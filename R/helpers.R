
# Make sure columns in an outcomes data-frame are present and correct types.
spruce_outcomes_df <- function(df) {
  if(is.list(df$dose)) {
    df$dose <- map(df$dose, as.integer)
    # Add dose_string column because, for filtering, dose must ultimately be
    # represented by a scalar primitive type:
    df <-
      df %>%
      mutate(
        dose_string = map_chr(dose, dose_vector_to_string)
      )
  } else {
    df$dose <- as.integer(df$dose)
  }
  df$tox <- as.integer(df$tox)
  if('cohort' %in% colnames(df)) df$cohort <- as.integer(df$cohort)
  if('patient' %in% colnames(df)) df$patient <- as.integer(df$patient)
  if('eff' %in% colnames(df)) df$eff <- as.integer(df$eff)
  df
}


#' Get all combinations of dose indices
#'
#' @param num_doses integer vector of number of doses for length(num_doses)
#'   treatments
#'
#' @return a list, each element being an integer vector
#'
#' @importFrom purrr map reduce pmap
#' @importFrom tidyr expand_grid
#'
#' @examples
#' get_dose_combo_indices(num_doses = c(1, 2))
#' # returns list(c(1, 1), c(1, 2))
get_dose_combo_indices <- function(num_doses) {
  return(
    map(num_doses, seq_len) %>%
      reduce(expand_grid) %>%
      pmap(.f = function(...) as.integer(c(...)))
  )
}

#' @importFrom tibble tibble
#' @importFrom purrr map_int map_chr
model_frame_to_counts <- function(model_frame, num_doses) {

  if(any(num_doses <= 0)) {

    # What are we doing here??
    df_c <- tibble(
      dose = integer(length = 0),
      n = integer(length = 0),
      tox = integer(length = 0)
    )
    if('eff' %in% colnames(df)) {
      df_c$eff <- integer(length = 0)
    }

  } else if(length(num_doses) == 1) {

    # Single treatment scenario
    df <- model_frame
    dose_indices <- 1:num_doses
    dose_counts <- map_int(dose_indices, ~ sum(df$dose == .x))
    tox_counts <- map_int(dose_indices, ~ sum(df$tox[df$dose == .x]))
    df_c <- tibble(
      dose = dose_indices,
      n = dose_counts,
      tox = tox_counts
    )
    if('eff' %in% colnames(df)) {
      df_c$eff <- map_int(dose_indices, ~ sum(df$eff[df$dose == .x]))
    }

  } else {

    # Multi-treatment scenario
    df <- model_frame
    dose_indices <- get_dose_combo_indices(num_doses)
    dose_string <- map_chr(dose_indices, dose_vector_to_string)
    dose_counts <- map_int(dose_string, ~ sum(df$dose_string == .x))
    tox_counts <- map_int(dose_string, ~ sum(df$tox[df$dose_string == .x]))
    df_c <- tibble(
      dose = dose_indices,
      dose_string = dose_string,
      n = dose_counts,
      tox = tox_counts
    )
    if('eff' %in% colnames(df)) {
      df_c$eff <- map_int(dose_string, ~ sum(df$eff[df$dose_string == .x]))
    }

  }

  return(df_c)
}

# Copied from BOIN package (https://cran.r-project.org/package=BOIN) to
# perform isotonic regression in order to work with their method.
pava <- function(x, wt = rep(1, length(x))) {
  n <- length(x)
  if (n <= 1)
    return(x)
  if (any(is.na(x)) || any(is.na(wt))) {
    stop("Missing values in 'x' or 'wt' is not allowed")
  }
  lvlsets <- 1:n
  repeat {
    viol <- as.vector(diff(x)) < 0
    if(!(any(viol)))
      return(x)

    i <- min((1:(n - 1))[viol])
    lvl1 <- lvlsets[i]
    lvl2 <- lvlsets[i + 1]
    ilvl <- (lvlsets == lvl1 | lvlsets == lvl2)
    x[ilvl] <- sum(x[ilvl] * wt[ilvl])/sum(wt[ilvl])
    lvlsets[ilvl] <- lvl1
  }
  x
}

#' @importFrom stats pbeta
#' @importFrom purrr map_dbl
pava_bb_prob_tox_exceeds <- function(x, threshold, alpha, beta, ...) {

  # PAVA-smoothed estimates of the probability that toxicity exceeds threshold
  # using a beta-binomial inference model.

  prob_od <- pbeta(q = threshold,
                   shape1 = alpha + tox_at_dose(x),
                   shape2 = beta + n_at_dose(x) - tox_at_dose(x),
                   lower.tail = FALSE)
  names(prob_od) <- dose_indices(x)

  # Apply isotonic regression to just those doses given
  given <- n_at_dose(x) > 0
  prob_od2 <- pava(prob_od[given])

  # Mask as NA those dose not given:
  prob_od3 <- map_dbl(
    1:num_doses(x),
    ~ ifelse(.x %in% names(prob_od2), prob_od2[as.character(.x)], NA)
  )

  return(prob_od3)
}

#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr mutate group_by slice ungroup select arrange
#' @importFrom tidyr unnest
reverse_engineer_prob_tox_quantile <- function(
    x, p, quantile_candidates = seq(0, 1, length.out = 101), ...) {

  # In dose_selectors, prob_tox_quantile and prob_tox_exceeds should generally
  # be in agreement, similar to how the q() and p() distribution functions in R
  # are in agreement.
  # Sometimes ensuring this whilst calculating quantiles is difficult,
  # suggesting the problem could be seen as an inverse problem.
  # I.e. choose the quantile from the list of candidates at a given granularity
  # that closest matches given the probabilities yielded by prob_tox_exceeds.

  dose <- prob <- . <- distance <- NULL
  df <- tibble(
    q = quantile_candidates
  ) %>% mutate(
    dose = map(q, .f = ~ dose_indices(x)),
    prob = map(q, .f = ~ 1 - prob_tox_exceeds(x, threshold = .x))
  ) %>% unnest(cols = c(dose, prob)) %>%
    mutate(distance = abs(prob - p)) %>%
    arrange(dose, distance) %>%
    group_by(dose) %>%
    slice(1) %>%
    ungroup() %>%
    select(q) %>% .[[1]]
  df[n_at_dose(x) == 0] <- NA
  names(df) <- dose_indices(x)
  df
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
  time_delta <- rexp(n = n, rate = 1 / mean_time_delta)
  data.frame(time_delta = time_delta)
}


#' Go from a single multi-treatment vector of dose-indices to a dose string
#'
#' @param x vector of integer dose indices
#'
#' @return character
#'
#' @export
#'
#' @examples
#' dose_vector_to_string(as.integer(c(1, 3))) # The indices must be integers!
dose_vector_to_string <- function(x) {
  if(!is.integer(x)) {
    stop("x should be of type integer")
  }
  if(any(x <= 0)) {
    stop("x should be strictly positive")
  }
  return(paste(x, collapse = "."))
}

#' Go from a single multi-treatment dose string to a vector of dose-indices
#'
#' @param x a multi-treatment dose string like "1.3" or "3.2"
#'
#' @return a vector of integer dose-indices
#' @export
#'
#' @examples
#' dose_string_to_vector("1.3")
dose_string_to_vector <- function(x) {
  y <- strsplit(x, "\\.")
  if(length(y) == 1) {
    return(as.integer(y[[1]]))
  } else {
    stop(paste0("failed to parse '", x, "'"))
  }
}
