
#' Calculate future dose paths.
#'
#' A dose-escalation design exists to select doses in response to observed
#' outcomes. The entire space of possible responses can be calculated to show
#' the behaviour of a design in response to all feasible outcomes. This function
#' performs that task.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#' @param cohort_sizes Integer vector representing sizes of
#' @param ... Extra args are passed onwards.
#'
#' @return Object of type \code{\link{dose_paths}}.
#' @export
#'
#' @examples
#' # Calculate paths for a 3+3 design for the next two cohorts of three patients
#' paths <- get_three_plus_three(num_doses = 5) %>%
#'   get_dose_paths(cohort_sizes = c(3, 3))
get_dose_paths <- function(selector_factory, cohort_sizes, ...) {
  dose_paths_func <- selector_factory %>% dose_paths_function()
  dose_paths_func(selector_factory, cohort_sizes = cohort_sizes, ...)
}
