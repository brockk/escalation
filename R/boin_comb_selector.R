
#' Get an object to fit the BOIN COMB model using the BOIN package.
#'
#' @param num_doses integer vector of the number of doses of treatment 1, 2
#' @param target We seek a dose with this probability of toxicity.
#' @param use_stopping_rule TODO
#' @param ... Extra args are passed to \code{\link[BOIN]{next.comb}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' BOIN COMB model to outcomes.
#'
#' @export
#'
#' @examples
#' num_doses <- c(3, 4)
#' target <- 0.25
#' boin_fitter <- get_boin_comb(num_doses = num_doses, target = target)
#' x1 <- fit(boin_fitter, outcomes = "1.1NNN")
#' x1
#' x2 <- fit(boin_fitter, outcomes = "1.1NNN 2.1TNT")
#' x2
#'
#' @references
#' Lin, R., & Yin, G. (2017).
#' Bayesian optimal interval design for dose finding in drug-combination trials.
#' Statistical methods in medical research, 26(5), 2155-2167.
get_boin_comb <- function(num_doses, target, use_stopping_rule = TRUE, ...) {

  if(length(num_doses) <= 1) {
    stop("Expecting num_doses to be at least of length 2.")
  }

  x <- list(
    num_doses = num_doses,
    target = target,
    use_stopping_rule = use_stopping_rule,
    extra_args = list(...)
  )

  class(x) <- c(
    "boin_comb_selector_factory",
    "combo_selector_factory",
    "tox_selector_factory",
    "selector_factory"
  )
  return(x)
}

#' @importFrom BOIN next.comb
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom purrr map map_lgl
boin_comb_selector <- function(outcomes, num_doses, target, use_stopping_rule,
                               ...) {

  # Build NOTES etc
  dose_string <- NULL

  # Checks
  if(length(num_doses) <= 1) {
    stop("Expecting num_doses to be at least of length 2.")
  }
  num_treatments <- length(num_doses)

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  df_c <- model_frame_to_counts(df, num_doses = num_doses)
  z <- .outcomes_to_arrays(df, num_doses = num_doses)

  # Checks
  if(nrow(df) > 0) {
    dose_outside_bounds <-
      map(df$dose, ~ .x > num_doses) %>%
      map_lgl(any) %>%
      any()
    if(dose_outside_bounds) {
      stop("boin_comb_selector - dose given exceeds the maximum.")
    }
  }

  if(nrow(df) == 0) {
    recommended_dose <- rep(1, num_treatments)
    continue <- TRUE
    x <- NULL
    bound <- NULL
  } else {
    last_dose_string <- tail(df, 1) %>% pull(dose_string)
    last_dose <- dose_string_to_vector(last_dose_string)
    x <- next.comb(target = target, npts = z$num_patients, ntox = z$num_tox,
                   dose.curr = last_dose, ...)
    recommended_dose <- as.integer(x$next_dc)
    continue <- !any(is.na(x$next_dc))

    # n_d <- df_c$n[last_dose]
    # tox_d <- df_c$tox[last_dose]

    # Note ... params shared with next.comb above
    # ..+2 to stave off error in BOIN if cohortsize == 1:
    bound <- get.boundary(target = target, ncohort = 1,
                          cohortsize = nrow(df) + 2, ...)
    # this_bound <- bound$full_boundary_tab[, n_d]
    # if(tox_d <= this_bound['Escalate if # of DLT <=']) {
    #   # The trial can continue
    #   continue <- TRUE
    #   # Escalate, if possible
    #   recommended_dose <- pmin(num_doses, last_dose + 1)
    #   # But check recommended_dose is not eliminated, if using stopping rule
    #   n_r <- df_c$n[recommended_dose]
    #   if(use_stopping_rule & n_r > 0) {
    #     rec_bound <- get.boundary(target = target, ncohort = 1,
    #                               cohortsize = n_r + 1, ...)
    #     this_rec_bound <- rec_bound$full_boundary_tab[, n_r]
    #     tox_r <- df_c$tox[recommended_dose]
    #
    #     if( !is.na(this_rec_bound['Eliminate if # of DLT >=']) &
    #         tox_r >= this_rec_bound['Eliminate if # of DLT >=']) {
    #       # Do not escalate. Stay at last_dose
    #       recommended_dose <- last_dose
    #     }
    #   }
    #
    # } else if(tox_d >= this_bound['Deescalate if # of DLT >=']) {
    #   # De-escalate and possibly eliminate
    #   if(use_stopping_rule &
    #      !is.na(this_bound['Eliminate if # of DLT >=']) &
    #      tox_d >= this_bound['Eliminate if # of DLT >=']){
    #
    #     # If elimination is path-dependent this approach could fail.
    #
    #     if(last_dose == 1) {
    #       # Stop and recommend no dose.
    #       recommended_dose <- NA
    #       continue <- FALSE
    #     } else {
    #       # Eliminate this dose and all higher doses.
    #       # Select highest non-elimnated dose
    #       recommended_dose <- last_dose - 1
    #       continue <- TRUE
    #     }
    #   } else {
    #     # De-escalate, if possible
    #     recommended_dose <- pmax(1, last_dose - 1)
    #     continue <- TRUE
    #   }
    # } else {
    #   # As you were
    #   recommended_dose <- last_dose
    #   continue <- TRUE
    # }
  }

  l <- list(
    cohort = df$cohort,
    df = df,
    num_doses = as.integer(num_doses),
    target = target,
    boin_fit = x,
    bound = bound,
    use_stopping_rule = use_stopping_rule,


    # bound = get.boundary(target = target, ncohort = 1,
    #                      cohortsize = nrow(df) + 2, ...),

    df = df,
    df_c = df_c,
    recommended_dose = recommended_dose,
    continue = continue
  )

  class(l) = c(
    "boin_comb_selector",
    "combo_selector",
    "tox_selector",
    "selector"
  )
  l
}

# Factory interface

#' @export
fit.boin_comb_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    target = selector_factory$target,
    use_stopping_rule = selector_factory$use_stopping_rule
  )
  args <- append(args, selector_factory$extra_args)
  do.call(boin_comb_selector, args = args)
}

# Selector interface

#' @export
model_frame.boin_comb_selector <- function(x, ...) {
  return(x$df)
}

#' @export
tox_target.boin_comb_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.boin_comb_selector <- function(x, ...) {
  return(nrow(x$df))
}

#' @export
cohort.boin_comb_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.boin_comb_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.boin_comb_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
eff.boin_comb_selector <- function(x, ...) {
  if("eff" %in% colnames(x$df)) {
    return(x$df$eff)
  } else {
    return(rep(NA, num_patients(x)))
  }
}

#' @export
num_doses.boin_comb_selector <- function(x, ...) {
  return(x$num_doses)
}

#' @export
recommended_dose.boin_comb_selector <- function(x, ...) {
  return(x$recommended_dose)
}

#' @export
continue.boin_comb_selector <- function(x, ...) {
  return(x$continue)
}

#' @importFrom Iso biviso
#' @export
mean_prob_tox.boin_comb_selector <- function(x, ...) {
  # The authors use beta-binomial conjugate approach with a Beta(0.05, 0.05)
  # prior, and then 2-d isotonic regression to ensure increasing estimates:
  phat = (tox_at_dose(x) + 0.05) / (n_at_dose(x) + 0.1)
  phat = biviso(phat, n_at_dose(x) + 0.1, warn = TRUE)
  return(phat[, ]) # The [, ] strips extraneous attributes.
}

#' @rdname median_prob_tox
#' @param iso TRUE to use isotonic regression on the posterior medians; FALSE
#' to return just the posterior medians, which may not be monotonically
#' increasing by dose.
#' @export
median_prob_tox.boin_comb_selector <- function(x, iso = TRUE, ...) {
  # The authors use beta-binomial conjugate approach with a Beta(0.05, 0.05)
  # prior, and then 2-d isotonic regression to ensure increasing estimates.
  prob_tox_quantile(x, p = 0.5, iso = iso, ...)
}

#' @export
dose_admissible.boin_comb_selector <- function(x, ...) {

  n_d <- n_at_dose(x)
  reject <- matrix(FALSE, nrow = nrow(n_d), ncol = ncol(n_d))
  if(x$use_stopping_rule) {
    t_d <- tox_at_dose(x)
    for(i in seq_len(nrow(n_d))) {
      for(j in seq_len(ncol(n_d))) {
        if(n_d[i, j] > 0) {
          this_bound <- x$bound$full_boundary_tab[, n_d[i, j]]
          boundary_t <- this_bound['Eliminate if # of DLT >=']
          if(is.na(boundary_t)) {
            reject[i, j] <- FALSE # Implicitly
          } else {
            reject[i, j] <- t_d[i, j] >= boundary_t # Explicitly
          }
        } else {
          reject[i, j] <- FALSE # Implicitly
        }
      }
    }

    # However, monotonic tox suggests doses higher than an inadmissible dose
    # are also inadmissible.
    # Propagate rejection rightwards across rows (i.e. towards higher doses)
    for(i in seq_len(nrow(n_d))) {
      reject[i, ] <- cumsum(reject[i, ]) >= 1
    }
    # Propagate rejection down cols (i.e. towards higher doses)
    for(j in seq_len(ncol(n_d))) {
      reject[, j] <- cumsum(reject[, j]) >= 1
    }
  }

  return(!reject)
}

#' @rdname prob_tox_quantile
#' @param iso TRUE to use isotonic regression on the posterior quantiles; FALSE
#' to return just the posterior quantiles, which may not be monotonically
#' increasing by dose.
#' @importFrom Iso biviso
#' @export
prob_tox_quantile.boin_comb_selector <- function(x, p, iso = TRUE, ...) {
  # The authors use beta-binomial conjugate approach with a Beta(0.05, 0.05)
  # prior, and then 2-d isotonic regression to ensure increasing estimates.
  y <- tox_at_dose(x) + 0.05
  n <- n_at_dose(x) + 0.1
  q <- qbeta(p = p, shape1 = y, shape2 = n - y, lower.tail = TRUE)
  if(iso) {
    q_iso = biviso(q)
    return(q_iso[, ]) # The [, ] strips extraneous attributes.
  } else {
    return(q)
  }
}

#' @rdname prob_tox_exceeds
#' @param iso TRUE to use isotonic regression on the posterior probabilities;
#' FALSE to return just the posterior quantiles, which may not be monotonically
#' increasing by dose.
#' @importFrom Iso biviso
#' @export
prob_tox_exceeds.boin_comb_selector <- function(x, threshold, iso = TRUE, ...) {
  # The authors use beta-binomial conjugate approach with a Beta(0.05, 0.05)
  # prior, and then 2-d isotonic regression to ensure increasing estimates.
  y <- tox_at_dose(x) + 0.05
  n <- n_at_dose(x) + 0.1
  p <- pbeta(q = threshold, shape1 = y, shape2 = n - y, lower.tail = FALSE)
  if(iso) {
    p_iso = biviso(p)
    return(p_iso[, ]) # The [, ] strips extraneous attributes.
  } else {
    return(p)
  }
}

#' @export
supports_sampling.boin_comb_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.boin_comb_selector <- function(x, tall = FALSE, ...) {
  stop('boin_selector does not support sampling.')
}
