
#' @export
simulation_function.eff_tox_selector_factory <- function(selector_factory) {
  return(phase1_2_sim)
}

#' @export
dose_paths_function.eff_tox_selector_factory <- function(selector_factory) {
  return(phase1_2_dose_paths)
}
