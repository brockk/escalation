
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

#' @importFrom utils head
#' @export
num_doses.dose_paths <- function(x, ...) {
  # Have a word with this amount of nesting!
  if(length(x) > 0) {
    num_doses(head(x, 1)[[1]][['fit']])
  } else {
    return(0)
  }
}

#' @export
dose_indices.dose_paths <- function(x, ...) {
  n_d <- num_doses(x)
  if(n_d > 0) {
    1:n_d
  } else {
    integer(length = 0)
  }
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
    # outcomes = map_chr(~ as.character(.x$outcomes)),
    next_dose = map_dbl(x, 'next_dose'),
    fit = map(x, 'fit'),
    parent_fit = map(x, 'parent_fit'),
    dose_index = map(fit, 'dose_indices'),
    ...
  )
}

#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @export
print.dose_paths <- function(x, node = 1, ...) {

  .node <- .parent <- NULL

  df <- as_tibble(x)
  sub_df <- df %>% filter(.node == node)
  if(nrow(sub_df) == 0) invisible(NULL)
  outcomes <- sub_df[1, 'outcomes', drop = TRUE]
  node_id <- sub_df[1, '.node', drop = TRUE]
  next_dose <- sub_df[1, 'next_dose', drop = TRUE]
  depth <- sub_df[1, '.depth', drop = TRUE]
  if(node == 1) {
    cat(paste0(strrep('  ', depth), 'Give dose ', next_dose, ':'), '\n')
  } else {
    cat(paste0(strrep('  ', depth), outcomes, ' -> ', next_dose), '\n')
  }

  children <- df %>% filter(.parent == node_id)
  if(nrow(children) > 0) {
    for(i in children$.node) {
      print.dose_paths(x, node = i)
    }
  }
}
