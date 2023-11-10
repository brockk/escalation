
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
convergence_plot <- function(x) {
  num_doses(x[[1]])
  stack_sims_vert(x, target_dose = 2)
  cum_sum_df <- stack_sims_vert(x, target_dose = 2)
  # TODO
}
