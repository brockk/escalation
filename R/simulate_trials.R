
#' Simulate clinical trials.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#' @param num_sims Integer, number of trial iterations to simulate.
#' @param ... Extra args are passed onwards.
#'
#' @return Object of type \code{\link{simulations}}.
#' @export
#'
#' @examples
#' # Simulate performance of the 3+3 design:
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox)
simulate_trials <- function(selector_factory, num_sims, ...) {
  sim_func <- simulation_function(selector_factory)
  l <- lapply(1:num_sims, function(x) sim_func(selector_factory, ...))
  class(l) <- 'simulations'
  l
}
