
spruce_outcomes_df <- function(df) {
  df$dose <- as.integer(df$dose)
  df$tox <- as.integer(df$tox)
  if('cohort' %in% colnames(df)) df$cohort <- as.integer(df$cohort)
  if('patient' %in% colnames(df)) df$patient <- as.integer(df$patient)
  df
}


#' @importFrom tibble tibble
model_frame_to_counts <- function(model_frame, num_doses = NULL) {
  df <- model_frame
  if(is.null(num_doses)) num_doses <- max(df$dose)
  dose_indices <- 1:num_doses
  dose_counts <- map_int(dose_indices, ~ sum(df$dose == .x))
  tox_counts <- map_int(dose_indices, ~ sum(df$tox[df$dose == .x]))
  df_c <- tibble(dose = dose_indices, n = dose_counts, tox = tox_counts)

  if('eff' %in% colnames(df)) {
    df_c$eff <- map_int(dose_indices, ~ sum(df$eff[df$dose == .x]))
  }

  df_c
}

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
