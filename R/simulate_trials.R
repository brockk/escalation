
#' Simulate clinical trials.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#' @param num_sims integer, number of trial iterations to simulate.
#' @param true_prob_tox numeric vector of toxicity probabilities
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
simulate_trials <- function(selector_factory, num_sims, true_prob_tox, ...) {
  sim_func <- simulation_function(selector_factory)
  l <- lapply(1:num_sims, function(x) sim_func(selector_factory, true_prob_tox,
                                               ...))
  # l[['true_prob_tox']] <- true_prob_tox

  # class(l) <- 'simulations'
  # l

  simulations(fits = l, true_prob_tox = true_prob_tox)
}
