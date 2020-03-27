
#' @export
simulation_function.derived_dose_selector_factory <- function(selector_factory){
  return(simulation_function(selector_factory$parent))
}

#' @export
dose_paths_function.derived_dose_selector_factory <- function(selector_factory){
  return(dose_paths_function(selector_factory$parent))
}
