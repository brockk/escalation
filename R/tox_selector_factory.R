
#' @export
simulation_function.tox_selector_factory <- function(selector_factory) {
  return(phase1_sim)
}

#' @export
dose_paths_function.tox_selector_factory <- function(selector_factory) {
  return(phase1_dose_paths)
}
