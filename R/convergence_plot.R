
#' Plot the convergence processes from a collection of simulations.
#'
#' @param x object of type \code{\link{simulations_collection}}
#' @param ... extra args are passed onwards to stack_sims_vert
#'
#' @return a ggplot2 plot
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_line ylim facet_wrap labs theme
#' @export
#'
#' @examples
#' design1 <- get_mtpi2(
#'   num_doses = 5, target = 0.25,
#'   epsilon1 = 0.05, epsilon2 = 0.05,
#'   exclusion_certainty = 0.95
#' ) %>%
#'   stop_at_n(n = 12)
#' design2 <- get_mtpi2(
#'   num_doses = 5, target = 0.25,
#'   epsilon1 = 0.1, epsilon2 = 0.1,
#'   exclusion_certainty = 0.95
#' ) %>%
#'   stop_at_n(n = 12)
#' designs <- list(
#'   "Design1" = design1,
#'   "Design2" = design2
#' )
#' sims <- simulate_compare(
#'   designs = designs,
#'   num_sims = 20,
#'   true_prob_tox = c(0.1, 0.2, 0.3, 0.4, 0.5)
#' )
#' convergence_plot(sims)
convergence_plot <- function(x, ...) {

  # Avoid NOTEs
  n <- .rate <- design <- NULL

  stack_sims_vert(x, ...) %>%
    ggplot(aes(x = n, y = .rate, col = design)) +
    geom_line() +
    ylim(0, 1) +
    facet_wrap(~ dose, ncol = 5) +
    labs(title = "Convergence of simulation",
         x = "Iterate", y = "Prob(Selection)") +
    theme(legend.position = "bottom")
}
