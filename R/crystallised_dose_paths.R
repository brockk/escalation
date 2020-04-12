
#' Dose-paths with probabilities attached.
#'
#' \code{\link{dose_paths}} reflect all possible paths a dose-finding trial may
#' take. When the probability of those paths is calculated using an assumed set
#' of true dose-event probabilities, in this package those paths are said to be
#' crysallised. Once crystallised, operating charactersitics can be calculated.
#'
#' @param dose_paths Object of type \code{\link{dose_paths}}
#' @param true_prob_tox vector of probabilities
#' @param terminal_nodes tibble of terminal nodes on the dose-paths
#'
#' @return An object of type crystallised_dose_paths
#' @export
#'
#' @examples
#' # Calculate dose paths for the first three cohorts in a 3+3 trial of 5 doses:
#' paths <- get_three_plus_three(num_doses = 5) %>%
#'   get_dose_paths(cohort_sizes = c(3, 3, 3))
#'
#' # Set the true probabilities of toxicity
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' # Crytallise the paths with the probabilities of toxicity
#' x <- paths %>% calculate_probabilities(true_prob_tox)
#' # And then examine, for example, the probabilities of recommending each dose
#' # at the terminal nodes of these paths:
#' prob_recommend(x)
crystallised_dose_paths <- function(dose_paths, true_prob_tox,
                                    terminal_nodes) {
  l <- list(dose_paths = dose_paths,
            true_prob_tox = true_prob_tox,
            terminal_nodes = terminal_nodes)
  class(l) <- 'crystallised_dose_paths'
  l
}

#' @importFrom dplyr mutate filter summarise
#' @importFrom purrr map_int
#' @export
prob_recommend.crystallised_dose_paths <- function(x, ...) {

  prob_outcomes <- . <- NULL

  df <- x$terminal_nodes %>%
    mutate(recommended_dose = map_int(fit, recommended_dose))
  prob_stop <- df %>% filter(is.na(recommended_dose)) %>%
    summarise(prob = sum(prob_outcomes)) %>% .[[1]]

  .get_prob_d <- function(i) {
    . <- prob_outcomes <- recommended_dose <- NULL
    df %>%
      filter(recommended_dose == i) %>%
      summarise(prob = sum(prob_outcomes)) %>% .[[1]]
  }

  prob_d <- sapply(dose_indices(x$dose_paths), .get_prob_d)
  prob_rec <- c(prob_stop, prob_d)
  names(prob_rec) <- c('NoDose', dose_indices(x$dose_paths))
  prob_rec
}

#' @importFrom dplyr mutate group_by summarise
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @export
prob_administer.crystallised_dose_paths <- function(x, ...) {

  prob_outcomes <- dose <- prob_admin_d <- scaled_prob_administer <- NULL
  . <- NULL

  df <- x$terminal_nodes %>%
    mutate(
      dose = map(fit, dose_indices),
      prob_admin_d = map(fit, prob_administer)
    )

  grouped_df <- df %>%
    unnest(c(prob_outcomes, dose, prob_admin_d)) %>%
    mutate(scaled_prob_administer = prob_outcomes * prob_admin_d) %>%
    group_by(dose) %>%
    summarise(prob_admin_d = sum(scaled_prob_administer))

  prob_admin <- grouped_df %>% .[[2]]
  names(prob_admin) <- grouped_df %>% .[[1]]
  prob_admin
}
