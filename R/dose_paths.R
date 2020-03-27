
#' Dose pathways
#'
#' @description
#' A dose-escalation design exists to select doses in response to observed
#' outcomes. The entire space of possible responses can be calculated to show
#' the behaviour of a design in response to all feasible outcomes. The
#' \code{\link{get_dose_paths}} function performs that task and returns an
#' instance of this object.
#'
#' @seealso \code{\link{selector}}
#'
#' @export
#'
#' @examples
#'
#' # Calculate dose-paths for the 3+3 design:
#' paths <- get_three_plus_three(num_doses = 5) %>%
#'   get_dose_paths(cohort_sizes = c(3, 3))
dose_paths <- function() {
  # This function exists only to document the class "dose_paths".
}

#' Cast \code{\link{dose_paths}} object to \code{\link[tibble]{tibble}}.
#'
#' @param x Object of class \code{dose_finding_paths}.
#' @param ... Extra args passed onwards.
#'
#' @return Object of class \code{\link[tibble]{tibble}}
#'
#' @importFrom tibble tibble
#' @importFrom purrr map map_dbl map_chr
#' @export
as_tibble.dose_paths <- function(x, ...) {
  fit <- NULL
  tibble(
    .node = map_dbl(x, '.node'),
    .parent = map_dbl(x, '.parent'),
    .depth = map_dbl(x, '.depth'),
    outcomes = map_chr(x, 'outcomes'),
    next_dose = map_dbl(x, 'next_dose'),
    fit = map(x, 'fit'),
    parent_fit = map(x, 'parent_fit'),
    dose_index = map(fit, 'dose_indices'),
    ...
  )
}
