
#' Calculate dose-path probabilities
#'
#' @description
#' Crystallise a set of \code{\link{dose_paths}} with probabilities to calculate
#' how likely each path is. Once probabilised in this way, the probabilities of
#' the terminal nodes in this set of paths will sum to 1. This allows users to
#' calculate operating characteristics.
#'
#' @param dose_paths Object of type \code{\link{dose_paths}}
#' @param true_prob_tox Numeric vector, true probability of toxicity.
#' @param true_prob_eff vector of true efficacy probabilities, optionally NULL
#' if efficacy not analysed.
#'
#' @seealso \code{\link{dose_paths}}
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate filter left_join case_when
#' @importFrom stringr str_count
#' @importFrom purrr map2 map_dfr
#' @importFrom stats dbinom
#' @export
#'
#' @examples
#' # Calculate dose paths for the first three cohorts in a 3+3 trial of 5 doses:
#' paths <- get_three_plus_three(num_doses = 5) %>%
#'   get_dose_paths(cohort_sizes = c(3, 3, 3))
#'
#' # Set the true probabilities of toxicity
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' # And calculate exact operating performance
#' x <- paths %>% calculate_probabilities(true_prob_tox)
#' prob_recommend(x)
calculate_probabilities <- function(dose_paths, true_prob_tox,
                                    true_prob_eff = NULL) {

  .node <- .parent <- prob_outcomes <- fit <- parent_fit <- NULL
  dose <- next_dose <- prob_tox <- outcomes <- NULL

  if(is.null(true_prob_eff)) {
    # Classic phase I trial, path probability determined by prob_tox only
    dose_df <- tibble(
      dose = dose_indices(dose_paths),
      prob_tox = true_prob_tox
    )
    paths_df <- as_tibble(dose_paths)
    paths_df <- paths_df %>%
      left_join(paths_df %>% select(.node, dose = next_dose),
                by = c('.parent' = '.node')) %>%
      left_join(dose_df, by = 'dose') %>%
      mutate(
        n = nchar(outcomes),
        n_tox = str_count(outcomes, 'T'),
        prob_outcomes = case_when(
          outcomes == '' ~ 1,
          TRUE ~ dbinom(x = n_tox, size = n, prob = prob_tox)
        )
      ) %>%
      select(.node, .parent, prob_outcomes, fit, parent_fit)
  } else {
    # Phase I/II trial, path probability determined by joint prob_tox & prob_eff
    dose_df <- tibble(
      dose = dose_indices(dose_paths),
      prob_tox = true_prob_tox,
      prob_eff = true_prob_eff
    )
    paths_df <- as_tibble(dose_paths)
    paths_df <- paths_df %>%
      left_join(paths_df %>% select(.node, dose = next_dose),
                by = c('.parent' = '.node')) %>%
      left_join(dose_df, by = 'dose') %>%
      mutate(
        n = nchar(outcomes),
        n_b = str_count(outcomes, 'B'),
        n_e = str_count(outcomes, 'E'),
        n_n = str_count(outcomes, 'N'),
        n_t = str_count(outcomes, 'T'),
        p_b = prob_tox * prob_eff,
        p_e = (1 - prob_tox) * prob_eff,
        p_n = (1 - prob_tox) * (1 - prob_eff),
        p_t = prob_tox * (1 - prob_eff),
        prob_outcomes = case_when(
          outcomes == '' ~ 1,
          # Multinomial PMF, logged for numerical stability:
          TRUE ~ exp(log(factorial(n)) +
                       n_b * log(p_b) + n_e * log(p_e) +
                       n_n * log(p_n) + n_t * log(p_t) -
                       log(factorial(n_b)) - log(factorial(n_e)) -
                       log(factorial(n_n)) - log(factorial(n_t)))
        )
      ) %>%
      select(.node, .parent, prob_outcomes, fit, parent_fit)
  }

  .recurse <- function(node_id, prob = 1) {
    children <- paths_df %>% filter(.parent == node_id)
    if(nrow(children) == 0) {
      return(paths_df %>% filter(.node == node_id) %>%
               mutate(prob_outcomes = prob))
    } else {
      map2(.x = children$.node, .y = children$prob_outcomes,
           .f = function(x1, x2) {
             .recurse(node_id = x1, prob = prob * x2)
           }) %>% map_dfr(rbind)
    }
  }

  min_node_id <- min(paths_df$.node)
  terminal_nodes = .recurse(node_id = min_node_id)

  crystallised_dose_paths(dose_paths = dose_paths,
                          true_prob_tox = true_prob_tox,
                          true_prob_eff = true_prob_eff,
                          terminal_nodes = terminal_nodes)
}
