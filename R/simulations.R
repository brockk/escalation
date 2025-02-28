
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
#'   \item \code{\link{dose_strings}}
#'   \item \code{\link{doses_given}}
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
#' sims %>% dose_strings()
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
  # TODO - this amount of nesting is nauseating
  num_doses(head(x$fits, 1)[[1]][[1]]$fit)
}

#' @export
dose_indices.simulations <- function(x, ...) {
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    return(seq_len(n_d))
  } else {
    return(get_dose_combo_indices(num_doses(x)))
  }
}

#' @importFrom purrr map_chr
#' @export
dose_strings.simulations <- function(x, ...) {
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    return(as.character(seq_len(n_d)))
  } else {
    # Combination study
    return(
      map_chr(
        get_dose_combo_indices(num_doses(x)),
        dose_vector_to_string
      )
    )
  }
}

#' @rdname doses_given
#' @param dose_string TRUE to return vector of character dose-strings; FALSE
#' (the default) to get a list of matrices, one for each simulated trial, with
#' the dose-indices of the different treatments in columns and patients in rows.
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @export
doses_given.simulations <- function(x, dose_strings = FALSE, ...) {
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    return(
      x$fits %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map("fit") %>%
        map(doses_given) %>%
        reduce(rbind) %>%
        unname()
    )
  } else {
    # Combination study
    if(dose_strings) {
      return(
        x$fits %>%
          map(~ tail(.x, 1)[[1]]) %>%
          map("fit") %>%
          map(doses_given, dose_string = TRUE) %>%
          reduce(rbind) %>%
          unname()
      )
    } else {
      return(
        x$fits %>%
          map(~ tail(.x, 1)[[1]]) %>%
          map("fit") %>%
          map(doses_given)
      )
    }
  }
}

#' @rdname recommended_dose
#' @param dose_string TRUE to return vector of character dose-strings; FALSE
#' (the default) to get a numerical vector of recommended dose-indices in
#' monotherapy studies, or a matrix of recommended dose-indices in combination
#' studies with the different treatments in columns and simulated outcomes in
#' rows.
#' @importFrom purrr map map_int map_chr reduce
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @export
recommended_dose.simulations <- function(x, dose_string = FALSE, ...) {
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    if(dose_string) {
      x$fits %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map('fit') %>%
        map(recommended_dose) %>%
        map_chr(dose_vector_to_string)
    } else {
      x$fits %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map('fit') %>%
        map_int(recommended_dose)
    }
  } else {
    # Combination study
    if(dose_string) {
      fits <- x$fits %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map('fit')
      # For some reason, fits %>% map(recommended_dose) gives unexpected output.
      # So:
      lapply(fits, recommended_dose) %>%
        map_chr(dose_vector_to_string)
    } else {
      fits <- x$fits %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map('fit')
      # For some reason, fits %>% map(recommended_dose) gives unexpected output.
      # So:
      lapply(fits, recommended_dose) %>%
        reduce(rbind) %>%
        unname()
    }
  }
}

#' @importFrom purrr map imap_int map2_int
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export
n_at_dose.simulations <- function(x, dose = NULL, ...) {

  n_at_d <- x$fits %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map(n_at_dose)
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    df <- n_at_d %>%
      do.call(what = rbind)
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
  } else {
    # Combination study
    if(is.null(dose)) {
      return(n_at_d)
    } else if(dose == 'recommended') {
      fits <- x$fits %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map('fit')
      rec_d <- lapply(fits, recommended_dose)
      # Pluck out patient count at rec-d with code like:
      # n_at_d[[1]][t(cbind(rec_d[[1]]))]
      map2_int(n_at_d, rec_d, ~ .x[t(cbind(.y))])
    } else {
      stop(paste0("Don't know what to do with dose = '", dose, "'"))
    }
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
  tox_d <- x$fits %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map(tox_at_dose)
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    df <- tox_d %>%
      do.call(what = rbind)
    colnames(df) <- dose_indices(x)
    df %>% as_tibble()
  } else {
    # Combination study
    return(tox_d)
  }
}

#' @importFrom purrr map_int reduce
#' @export
num_tox.simulations <- function(x, ...) {
  # tox_d <- x$fits %>%
  #   map(~ tail(.x, 1)[[1]]) %>%
  #   map('fit') %>%
  #   map(tox_at_dose)
  tox_d <- tox_at_dose(x)
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    rowSums(tox_d)
  } else {
    # Combination study
    # reduce(tox_d, `+`)
    map_int(tox_d, sum)
  }
}

#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export
eff_at_dose.simulations <- function(x, ...) {

  n_d <- num_doses(x)
  supports_efficacy <- ifelse(
    'supports_efficacy' %in% names(x),
    x$supports_efficacy,
    FALSE
  )
  if(supports_efficacy) {
    eff_d <- x$fits %>%
      map(~ tail(.x, 1)[[1]]) %>%
      map('fit') %>%
      map(eff_at_dose)

    if(length(n_d) == 1) {
      # Monotherapy study
      df <- eff_d %>%
        do.call(what = rbind)
      colnames(df) <- dose_indices(x)
      df %>% as_tibble()
    } else {
      # Combination study
      return(eff_d)
    }
  } else {
    if(length(n_d) == 1) {
      # Monotherapy study
      matrix(nrow = length(x$fits), ncol = num_doses(x))
    } else {
      # Combination study
      map(seq_along(x$fits),
          ~ array(NA, dim = n_d))
    }
  }
}

#' @importFrom purrr map_int reduce
#' @export
num_eff.simulations <- function(x, ...) {
  # rowSums(eff_at_dose(x, ...))
  eff_d <- eff_at_dose(x)
  n_d <- num_doses(x)
  if(length(n_d) == 1) {
    # Monotherapy study
    rowSums(eff_d)
  } else {
    # Combination study
    # reduce(eff_d, `+`)
    map_int(eff_d, sum)
  }
}


#' @importFrom purrr map_int map_chr map
#' @importFrom magrittr %>%
#' @export
prob_recommend.simulations <- function(x, ...) {
  if(length(x$fits) > 0) {
    n_d <- num_doses(x)
    if(length(n_d) == 1) {
      # Monotherapy study
      rec_d <- recommended_dose(x)
      df <- c(
        sum(is.na(rec_d)),
        map_int(seq_len(n_d), ~ sum(rec_d == .x, na.rm = TRUE))
      )
      names(df) <- c("NoDose", seq_len(n_d))
      df / sum(df)
    } else {
      # Combination study
      rec_d_s <- recommended_dose(x, dose_string = TRUE)
      d_i <- dose_indices(x)
      d_i_s <- d_i %>%
        map_chr(dose_vector_to_string)
      p_r <- c(
        sum(str_detect(rec_d_s, "NA")),
        map_int(d_i_s, ~ sum(rec_d_s == .x))
      )
      p_r <- p_r / sum(p_r)
      tibble(
        dose = c("NoDose", d_i),
        dose_string = c("NoDose", d_i_s),
        prob_recommend = p_r
      )
    }
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
    n_d <- num_doses(x)
    if(length(n_d) == 1) {
      # Monotherapy study
      if(method == 0) {
        total_n_at_dose <- n_at_dose(x) %>% colSums()
        names(total_n_at_dose) <- seq_len(n_d)
        total_n_at_dose / sum(total_n_at_dose)
      } else if(method == 1) {
        x$fits %>%
          map(~ tail(.x, 1)[[1]]) %>%
          map("fit") %>%
          map(prob_administer) %>%
          do.call(what = rbind)
      } else {
        return(NULL)
      }
    } else {
      # Combination study
      if(method == 0) {
        total_n_at_dose <-
          n_at_dose(x) %>%
          reduce(`+`)
        total_n_at_dose / sum(total_n_at_dose)
      } else if(method == 1) {
        x$fits %>%
          map(~ tail(.x, 1)[[1]]) %>%
          map("fit") %>%
          map(prob_administer)
      } else {
        return(NULL)
      }
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
    # map_chr('time') %>%
    map_chr(~ as.character(.x$time)) %>%
    as.numeric()
}

#' @export
print.simulations <- function(x, ...) {

  n_d <- num_doses(x)
  supports_efficacy <- ifelse(
    'supports_efficacy' %in% names(x),
    x$supports_efficacy,
    FALSE
  )

  cat('Number of iterations:', length(x$fits), '\n')
  cat('\n')

  cat('Number of doses:', num_doses(x), '\n')
  cat('\n')

  ptox <- x$true_prob_tox
  if(length(n_d) == 1) {
    # Monotherapy study
    names(ptox) <- dose_strings(x)
  } else {
    # Combination study
  }
  cat('True probability of toxicity:\n')
  print(ptox, digits = 3)
  cat('\n')

  if(supports_efficacy) {
    peff <- x$true_prob_eff

    if(length(n_d) == 1) {
      # Monotherapy study
      names(peff) <- dose_strings(x)
    } else {
      # Combination study
    }
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

  if(supports_efficacy) {
    cat('Total efficacies:\n')
    print(summary(num_eff(x)))
    cat('\n')
  }

  cat('Trial duration:\n')
  print(summary(trial_duration(x)))
  cat('\n')
}

#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom purrr reduce
#' @importFrom dplyr select everything
#' @export
summary.simulations <- function(object, ...) {

  n_d <- num_doses(object)
  supports_efficacy <- ifelse(
    'supports_efficacy' %in% names(object),
    object$supports_efficacy,
    FALSE
  )
  dose_labs <- c('NoDose', dose_strings(object))

  if(length(n_d) == 1) {
    # Monotherapy study
    df <- tibble(
      dose = ordered(dose_labs, levels = dose_labs),
      tox = c(0, colMeans(tox_at_dose(object))),
      n = c(0, colMeans(n_at_dose(object))),
      true_prob_tox = c(0, object$true_prob_tox),
      prob_recommend = unname(prob_recommend(object)),
      prob_administer = c(0, prob_administer(object))
    )
  } else if(length(n_d) == 2) {
    # Dual-agent study
    # The below uses of t() only make sense in 2-d matrices, i.e. dual agent
    # It might also work in studies with three tmts and higher, but I have not
    # tested it.
    df <- tibble(
      dose = factor(dose_labs, levels = dose_labs),
      tox = c(0, t(reduce(tox_at_dose(object), `+`) / length(object))),
      n = c(0, t(reduce(n_at_dose(object), `+`) / length(object))),
      true_prob_tox = c(0, t(object$true_prob_tox)),
      prob_recommend = prob_recommend(object)$prob_recommend,
      prob_administer = c(0, t(prob_administer(object)))
    )
  } else {
    stop(
      "Simulation summary not implemented for combinations of more than 2 tmts"
    )
  }

  if(supports_efficacy) {
    if(length(n_d) == 1) {
      # Monotherapy study
      df$eff <- c(0, colMeans(eff_at_dose(object)))
      df$true_prob_eff <- c(0, object$true_prob_eff)
    } else if(length(n_d) == 2) {
      # Combination study
      df$eff <- c(0, t(reduce(eff_at_dose(object), `+`) / length(object)))
      df$true_prob_eff <- c(0, t(object$true_prob_eff))
    } else {
      stop(
        "Simulation summary not implemented for combinations of more than 2 tmts"
      )
    }
    df <- df %>%
      select(dose, tox, eff, n, true_prob_tox, true_prob_eff,
             prob_recommend, prob_administer, everything())
  }

  return(df)
}

#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom purrr imap_dfr map_dfr
#' @importFrom dplyr mutate select everything
#' @export
as_tibble.simulations <- function(x, ...) {

  supports_efficacy <- ifelse(
    'supports_efficacy' %in% names(x),
    x$supports_efficacy,
    FALSE
  )

  .iteration <- .depth <- time <- true_prob_tox <- true_prob_tox <- NULL

  if(supports_efficacy) {
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

