

#' @export
#' @importFrom tibble tibble
phase1_outcomes_to_counts <- function(outcomes, num_doses = NULL) {
  df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  if(is.null(num_doses)) num_doses <- max(df$dose)
  dose_indices <- 1:num_doses
  dose_counts <- map_int(dose_indices, ~ sum(df$dose == .x))
  tox_counts <- map_int(dose_indices, ~ sum(df$tox[df$dose == .x]))
  tibble(dose = dose_indices, n = dose_counts, tox = tox_counts)
}
