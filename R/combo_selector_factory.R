
#' @export
simulation_function.combo_selector_factory <- function(selector_factory) {
  return(phase1_2_comb_sim)
}

#' @export
dose_paths_function.combo_selector_factory <- function(selector_factory) {
  stop("Dose-paths for combination studies not yet implemented.")
}
