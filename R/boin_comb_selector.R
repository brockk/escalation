
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
#' # TODO
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
    # "combo_selector_factory", # TODO
    "tox_selector_factory", # TODO
    "selector_factory" # TODO
  )
  return(x)
}

#' @importFrom BOIN next.comb
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
boin_comb_selector <- function(outcomes, num_doses, target, use_stopping_rule,
                               ...) {

  # Checks
  if(length(num_doses) <= 1) {
    stop("Expecting num_doses to be at least of length 2.")
  }
  num_treatments <- length(num_doses)

  if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    # TODO
    stop('outcomes should be a data-frame.')
  }

  df_c <- model_frame_to_counts(df, num_doses = num_doses)
  z <- .outcomes_to_arrays(df, num_doses = num_doses)

  # Checks
  # if(nrow(df) > 0) {
  #   if(max(df$dose) > num_doses) {
  #     stop('boin_selector - maximum dose given exceeds number of doses.')
  #   }
  # }
  # TODO

  if(nrow(df) == 0) {
    recommended_dose <- rep(1, num_treatments)
    continue <- TRUE
  } else {
    last_dose_string <- tail(df, 1) %>% pull(dose_string)
    last_dose <- dose_string_to_vector(last_dose_string)
    x <- next.comb(target = target, npts = z$num_patients, ntox = z$num_tox,
                   dose.curr = last_dose, ...)
    # TODO continue?
    continue <- TRUE

    # n_d <- df_c$n[last_dose]
    # tox_d <- df_c$tox[last_dose]
    # bound <- get.boundary(target = target, ncohort = 1,
    #                       cohortsize = n_d + 1, ...)
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
    use_stopping_rule = use_stopping_rule,

    # # ..+2 to stave off error in BOIN if cohortsize == 1:
    # bound = get.boundary(target = target, ncohort = 1,
    #                      cohortsize = nrow(df) + 2, ...),

    df = df,
    df_c = df_c,
    recommended_dose = as.integer(x$next_dc),
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
  # if(num_patients(x) > 0) {
  #   tibble(
  #     patient = seq(1, num_patients(x)),
  #     cohort = cohort(x) %>% as.integer(),
  #     dose = doses_given(x) %>% as.integer(),
  #     tox = tox(x) %>% as.integer()
  #   )
  # } else {
  #   tibble(
  #     patient = integer(length = 0),
  #     cohort = integer(length = 0),
  #     dose = integer(length = 0),
  #     tox = integer(length = 0)
  #   )
  # }
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
  if("eff" %in% colnames(df)) {
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

#' #' @export
#' tox_at_dose.boin_comb_selector <- function(x, ...) {
#'   return(x$df_c$tox)
#' }

#' @export
mean_prob_tox.boin_comb_selector <- function(x, ...) {
  # # The authors store prob(DLT) as an ordered variable with the probs as levels:
  # # They also use '----' to show a dose has never been given.
  # mean_s <- as.character(x$boin_fit$p_est$phat)
  # mean_s[mean_s == '----'] <- NA
  # return(as.numeric(mean_s))

  # TODO
  stop("Not implemented yet")
}

#' @export
median_prob_tox.boin_comb_selector <- function(x, ...) {
  # prob_tox_quantile(x, p = 0.5, ...)

  # TODO
  stop("Not implemented yet")
}

#' @export
dose_admissible.boin_comb_selector <- function(x, ...) {
  # if(x$use_stopping_rule) {
  #   n_d <- n_at_dose(x)
  #   t_d <- tox_at_dose(x)
  #   reject <- logical(length = num_doses(x))
  #   for(i in seq_along(reject)) {
  #     if(n_d[i] > 0) {
  #       this_bound <- x$bound$full_boundary_tab[, n_d[i]]
  #       boundary_t <- this_bound['Eliminate if # of DLT >=']
  #       if(is.na(boundary_t))
  #         reject[i] <- FALSE # Implicitly
  #       else
  #         reject[i] <- t_d[i] >= boundary_t # Explicitly
  #     } else {
  #       reject[i] <- FALSE # Implicitly
  #     }
  #   }
  #   # However, monotonic tox suggests doses higher than an inadmissible dose
  #   # are also inadmissible:
  #   cum_reject <- cumsum(reject) >= 1
  #   return(!cum_reject)
  # } else {
  #   return(rep(TRUE, num_doses(x)))
  # }

  # TODO
  stop("Not implemented yet")
}

#' @export
prob_tox_quantile.boin_comb_selector <- function(
    x, p,
    quantile_candidates = seq(0, 1, length.out = 101), ...) {
  # reverse_engineer_prob_tox_quantile(x, p, quantile_candidates, ...)

  # TODO
  stop("Not implemented yet")
}

#' @export
prob_tox_exceeds.boin_comb_selector <- function(x, threshold, ...) {
  # # The authors use beta-binomial conjugate approach. They use a
  # # Beta(0.05, 0.05) prior but I could not find a justification for that.
  # # They also use isotonic regression to ensure increasing estimates:
  # pava_bb_prob_tox_exceeds(x, threshold, alpha = 0.05, beta = 0.05)

  # TODO
  stop("Not implemented yet")
}

#' @export
supports_sampling.boin_comb_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.boin_comb_selector <- function(x, tall = FALSE, ...) {
  stop('boin_selector does not support sampling.')
}
