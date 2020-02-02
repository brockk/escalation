
#' @export
spruce_outcomes_df <- function(df) {
  df$dose <- as.integer(df$dose)
  df$tox <- as.integer(df$tox)
  if('cohort' %in% colnames(df)) df$cohort <- as.integer(df$cohort)
  if('patient' %in% colnames(df)) df$patient <- as.integer(df$patient)
  df
}

#' @export
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
