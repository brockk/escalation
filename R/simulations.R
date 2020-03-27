
#' Simulated trials.
#'
#' @description
#' This class encapsulates that many notional or virtual trials can be
#' simulated. Each recommends a dose or doses, keeps track of how many
#' patients have been treated at what doses, what toxicity outcomes have been
#' seen, and whether a trial advocates continuing. We run simulations to learn
#' about the operating characteristics of a trial design.
#'
#' Computationally, the \code{simulations} class supports much of the same
#' interface as \code{\link{selector}}, and a little more.
#'
#' @details The \code{simulations} object implements the following functions:
#' \itemize{
#'   \item \code{\link{num_patients}}
#'   \item \code{\link{num_doses}}
#'   \item \code{\link{dose_indices}}
#'   \item \code{\link{n_at_dose}}
#'   \item \code{\link{tox_at_dose}}
#'   \item \code{\link{num_tox}}
#'   \item \code{\link{recommended_dose}}
#'   \item \code{\link{prob_administer}}
#'   \item \code{\link{prob_recommend}}
#'   \item \code{\link{trial_duration}}
#' }
#'
#' @seealso \code{\link{selector}}
#'
#' @export
#'
#' @examples
#'
#' # Simulate performance of the 3+3 design:
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox)
#' # The returned object has type 'simulations'. The supported interface is:
#' sims %>% num_patients()
#' sims %>% num_doses()
#' sims %>% dose_indices()
#' sims %>% n_at_dose()
#' sims %>% tox_at_dose()
#' sims %>% num_tox()
#' sims %>% recommended_dose()
#' sims %>% prob_administer()
#' sims %>% prob_recommend()
#' sims %>% trial_duration()
simulations <- function() {
  # This function exists only to document the class "simulations".
}


#' @importFrom purrr map map_int
#' @importFrom magrittr %>%
#' @export
num_patients.simulations <- function(x, ...) {
  x %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map_int(num_patients)
}

#' @importFrom utils head
#' @export
num_doses.simulations <- function(x, ...) {
  # Have a word with this amount of nesting!
  num_doses(head(x, 1)[[1]][[1]]$fit)
}

#' @export
dose_indices.simulations <- function(x, ...) {
  n_d <- num_doses(x)
  if(n_d > 0) {
    1:n_d
  } else {
    integer(length = 0)
  }
}

#' @importFrom purrr map map_int
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @export
recommended_dose.simulations <- function(x, ...) {
  x %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map_int(recommended_dose)
}

#' @importFrom purrr map imap_int
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export
n_at_dose.simulations <- function(x, dose = NULL, ...) {

  x %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map(n_at_dose) %>%
    do.call(what = rbind) -> df
  colnames(df) <- dose_indices(x)
  df <- df %>% as_tibble()

  if(is.null(dose)) {
    return(df)
  } else if(dose == 'recommended') {
    rec_d <- recommended_dose(x)
    return(imap_int(rec_d, ~ ifelse(is.na(.x), NA, df[.y, .x, drop = TRUE])))
  } else {
    stop(paste0("Don't know what to do with dose = '", dose, "'"))
  }

}

#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export
tox_at_dose.simulations <- function(x, ...) {
  x %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map(tox_at_dose) %>%
    do.call(what = rbind) -> df
  colnames(df) <- dose_indices(x)
  df %>% as_tibble()
}

#' @export
num_tox.simulations <- function(x, ...) {
  rowSums(tox_at_dose(x, ...))
}

#' @importFrom purrr map_int
#' @export
prob_recommend.simulations <- function(x, ...) {
  if(length(x) > 0) {
    n_doses <- num_doses(x)
    rec_d <- recommended_dose(x)
    df <- c(sum(is.na(rec_d)),
           map_int(1:n_doses, ~ sum(rec_d == .x, na.rm = TRUE)))
    names(df) <- c('NoDose', 1:n_doses)
    df / sum(df)
  } else {
    return(NULL)
  }
}

#' @importFrom utils head tail
#' @importFrom magrittr %>%
#' @importFrom purrr map_int
#' @export
prob_administer.simulations <- function(x, method = 0, ...) {
  if(length(x) > 0) {
    if(method == 0) {
      n_doses <- num_doses(x)
      total_n_at_dose <- n_at_dose(x) %>% colSums()
      names(total_n_at_dose) <- 1:n_doses
      total_n_at_dose / sum(total_n_at_dose)
    } else if(method == 1) {
      x %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map(prob_administer) %>%
        do.call(what = rbind)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom purrr map map_chr
#' @export
trial_duration.simulations <- function(x, method = 0, ...) {
  x %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map_chr('time') %>%
    as.numeric()
}

# #' @export
# print.simulations <- function(simulations, ...) {
#
# }
#
# #' @export
# summary.simulations <- function(object, ...) {
#
# }

#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom purrr imap_dfr map_dfr
#' @importFrom dplyr mutate select everything
#' @export
as_tibble.simulations <- function(x, ...) {
  .iteration <- .depth <- time <- NULL
  x %>%
    imap_dfr(.f = function(x, i) {
      map_dfr(x, function(y) {
        as_tibble(y$fit) %>%
          mutate(
            .iteration = i,
            .depth = y$.depth,
            time = y$time
          )
      })
    }) %>%
    select(.iteration, .depth, time, everything())
}
