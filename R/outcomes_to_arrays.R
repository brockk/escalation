
.outcomes_to_arrays <- function(df, num_doses) {

  # TODO Check dosed outside num_doses

  do_eff <- "eff" %in% colnames(df)

  num_dose_combos <- prod(num_doses)
  num_patients <- integer(length = num_dose_combos)
  num_tox <- integer(length = num_dose_combos)
  if(do_eff) {
    num_eff <- integer(length = num_dose_combos)
  }

  dose_combos <-
    map(num_doses, seq_len) %>%
    reduce(expand_grid)
  colnames(dose_combos) <- paste0("tmt", seq_along(num_doses))

  for(i in seq_len(nrow(dose_combos))) {

    dose_indices <- as.integer(dose_combos[i, ])
    ds <- dose_vector_to_string(dose_indices)
    this_dose_df <-
      df %>%
      filter(dose_string == ds)
    this_num_patients <- nrow(this_dose_df)
    this_num_tox <- sum(this_dose_df$tox)
    if(do_eff) {
      this_num_eff <- sum(this_dose_df$eff)
    }

    target_index <- dose_indices[1] +
      sum(head(cumprod(num_doses), -1) * tail(dose_indices - 1, -1))

    num_patients[target_index] <- this_num_patients
    num_tox[target_index] <- this_num_tox
    if(do_eff) {
      num_eff[target_index] <- this_num_eff
    }

  }

  z <- list(
    num_patients = array(num_patients, dim = num_doses),
    num_tox = array(num_tox, dim = num_doses)
  )
  if(do_eff) {
    z$num_eff <- array(num_eff, dim = num_doses)
  }

  return(z)
}
