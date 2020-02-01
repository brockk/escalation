

#' @export
#' @importFrom tibble tibble
phase1_outcomes_to_counts <- function(num_doses, outcomes) {
  df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  dose_indices <- 1:num_doses
  dose_counts <- map_int(dose_indices, ~ sum(df$dose == .x))
  tox_counts <- map_int(dose_indices, ~ sum(df$tox[df$dose == .x]))
  tibble(dose = dose_indices, n = dose_counts, tox = tox_counts)
}
