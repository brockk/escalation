
#' Simulated trials.
#'
#' @description
#' This class encapsulates that many notional or virtual trials can be
#' simulated. Each recommends a dose (or doses), keeps track of how many
#' patients have been treated at what doses, what toxicity outcomes have been
#' seen, and whether a trial advocates continuing, etc. We run simulations to
#' learn about the operating characteristics of a trial design.
#'
#' Computationally, the \code{simulations} class supports much of the same
#' interface as \code{\link{selector}}, and a little more.
#' Thus, many of the same generic functions are supported - see Examples.
#' However, compared to \code{\link{selector}}s, the returned objects reflect
#' that there are many trials instead of one, e.g. \code{num_patients(sims)},
#' returns as an integer vector the number of patients used in the simulated
#' trials.
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
#' @param fits Simulated model fits, arranged as list of lists.
#' @param true_prob_tox vector of true toxicity probabilities
#' @param true_prob_eff vector of true efficacy probabilities, optionally NULL
#' if efficacy not analysed.
#' @param ... Extra args
#'
#' @return list with slots: \code{fits} containing model fits;
#' and \code{true_prob_tox}, contianing the assumed true probability of
#' toxicity.
#'
#' @seealso \code{\link{selector}}
#' @seealso \code{\link{simulate_trials}}
#'
#' @export
#'
#' @examples
#'
#' # Simulate performance of the 3+3 design:
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)
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
#'
#' # Access the list of model fits for the ith simulated trial using:
#' i <- 1
#' sims$fits[[i]]
#' # and the jth model fit for the ith simulated trial using:
#' j <- 1
#' sims$fits[[i]][[j]]
#' # and so on.
simulations <- function(fits, true_prob_tox, true_prob_eff = NULL, ...) {
  # This function exists only to document the class "simulations".
  l <- list(fits = fits,
            true_prob_tox = true_prob_tox,
            supports_efficacy = !is.null(true_prob_eff),
            true_prob_eff = true_prob_eff)
  extra_args = list(...)
  l <- append(l, extra_args)
  class(l) <- 'simulations'
  l
}

#' @export
length.simulations <- function(x) {
  length(x$fits)
}

#' @importFrom purrr map map_int
#' @importFrom magrittr %>%
#' @export
num_patients.simulations <- function(x, ...) {
  x$fits %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map_int(num_patients)
}

#' @importFrom utils head
#' @export
num_doses.simulations <- function(x, ...) {
  # Have a word with this amount of nesting!
  num_doses(head(x$fits, 1)[[1]][[1]]$fit)
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
  x$fits %>%
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

  x$fits %>%
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

#' @export
n_at_recommended_dose.simulations <- function(x, ...) {
  return(n_at_dose(x, dose = 'recommended'))
}

#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export
tox_at_dose.simulations <- function(x, ...) {
  x$fits %>%
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

#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export
eff_at_dose.simulations <- function(x, ...) {
  if(x$supports_efficacy) {
    x$fits %>%
      map(~ tail(.x, 1)[[1]]) %>%
      map('fit') %>%
      map(eff_at_dose) %>%
      do.call(what = rbind) -> df
    colnames(df) <- dose_indices(x)
    df %>% as_tibble()
  } else {
    matrix(nrow = length(x$fits), ncol = num_doses(x))
  }
}

#' @export
num_eff.simulations <- function(x, ...) {
  rowSums(eff_at_dose(x, ...))
}


#' @importFrom purrr map_int
#' @export
prob_recommend.simulations <- function(x, ...) {
  if(length(x$fits) > 0) {
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
  if(length(x$fits) > 0) {
    if(method == 0) {
      n_doses <- num_doses(x)
      total_n_at_dose <- n_at_dose(x) %>% colSums()
      names(total_n_at_dose) <- 1:n_doses
      total_n_at_dose / sum(total_n_at_dose)
    } else if(method == 1) {
      x$fits %>%
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
  x$fits %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map_chr('time') %>%
    as.numeric()
}

#' @export
print.simulations <- function(x, ...) {

  cat('Number of iterations:', length(x$fits), '\n')
  cat('\n')

  cat('Number of doses:', num_doses(x), '\n')
  cat('\n')

  ptox <- x$true_prob_tox
  names(ptox) <- dose_indices(x)
  cat('True probability of toxicity:\n')
  print(ptox, digits = 3)
  cat('\n')

  if(x$supports_efficacy) {
    peff <- x$true_prob_eff
    names(peff) <- dose_indices(x)
    cat('True probability of efficacy:\n')
    print(peff, digits = 3)
    cat('\n')
  }

  cat('Probability of recommendation:\n')
  print(prob_recommend(x), digits = 3)
  cat('\n')

  cat('Probability of administration:\n')
  print(prob_administer(x), digits = 3)
  cat('\n')

  cat('Sample size:\n')
  print(summary(num_patients(x)))
  cat('\n')

  cat('Total toxicities:\n')
  print(summary(num_tox(x)))
  cat('\n')

  if(x$supports_efficacy) {
    cat('Total efficacies:\n')
    print(summary(num_eff(x)))
    cat('\n')
  }

  cat('Trial duration:\n')
  print(summary(trial_duration(x)))
  cat('\n')
}

#' @export
summary.simulations <- function(object, ...) {

  dose_labs <- c('NoDose', as.character(dose_indices(object)))

  if(object$supports_efficacy) {
    tibble(
      dose = ordered(dose_labs, levels = dose_labs),
      tox = c(0, colMeans(tox_at_dose(object))),
      eff = c(0, colMeans(eff_at_dose(object))),
      n = c(0, colMeans(n_at_dose(object))),
      true_prob_tox = c(0, object$true_prob_tox),
      true_prob_eff = c(0, object$true_prob_eff),
      prob_recommend = unname(prob_recommend(object)),
      prob_administer = c(0, prob_administer(object))
    )
  } else {
    tibble(
      dose = ordered(dose_labs, levels = dose_labs),
      tox = c(0, colMeans(tox_at_dose(object))),
      n = c(0, colMeans(n_at_dose(object))),
      true_prob_tox = c(0, object$true_prob_tox),
      prob_recommend = unname(prob_recommend(object)),
      prob_administer = c(0, prob_administer(object))
    )
  }
}

#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom purrr imap_dfr map_dfr
#' @importFrom dplyr mutate select everything
#' @export
as_tibble.simulations <- function(x, ...) {

  .iteration <- .depth <- time <- true_prob_tox <- true_prob_tox <- NULL

  if(x$supports_efficacy) {
    x$fits %>%
      imap_dfr(.f = function(batch, i) {
        map_dfr(batch, function(y) {
          as_tibble(y$fit) %>%
            mutate(
              .iteration = i,
              .depth = y$.depth,
              time = y$time,
              true_prob_tox = c(0, x$true_prob_tox),
              true_prob_eff = c(0, x$true_prob_eff)
            )
        })
      }) %>%
      select(.iteration, .depth, time, everything())
  } else {
    x$fits %>%
      imap_dfr(.f = function(batch, i) {
        map_dfr(batch, function(y) {
          as_tibble(y$fit) %>%
            mutate(
              .iteration = i,
              .depth = y$.depth,
              time = y$time,
              true_prob_tox = c(0, x$true_prob_tox)
            )
        })
      }) %>%
      select(.iteration, .depth, time, everything())
  }
}

