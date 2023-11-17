
#' Plot the convergence processes from a collection of simulations.
#'
#' @param x object of type \code{\link{simulations_collection}}
#'
#' @return a ggplot2 plot
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_line ylim facet_wrap labs theme
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
convergence_plot <- function(x, ...) {
  stack_sims_vert(x, ...) %>%
    ggplot(aes(x = n, y = .rate, col = design)) +
    geom_line() +
    ylim(0, 1) +
    facet_wrap(~ dose, ncol = 5) +
    labs(title = "Convergence of simulation",
         subtitle = "Shared patients",
         x = "Iterate", y = "Prob(Selection)") +
    theme(legend.position = "bottom")
}
