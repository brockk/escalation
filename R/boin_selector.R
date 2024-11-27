
#' Get an object to fit the BOIN model using the BOIN package.
#'
#' @param num_doses Number of doses under investigation.
#' @param target We seek a dose with this probability of toxicity.
#' @param use_stopping_rule TRUE to use the toxicity stopping rule described in
#' Yan et al. (2019). FALSE to suppress the authors' stopping rule, with the
#' assumption being that you will test the necessity to stop early in some other
#' way.
#' @param stop_when_deescalation_impossible TRUE to stop a trial and recommend
#' no dose when the advice is to de-escalate but de-escalation is impossible
#' because we are already at the lowest dose. Note that this feature was
#' requested by a user. This param is FALSE by default so that behaviour matches
#' what was described in the publication. The original authors do advocate this
#' behaviour.
#' @param ... Extra args are passed to \code{\link[BOIN]{get.boundary}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' BOIN model to outcomes.
#'
#' @export
#'
#' @examples
#' target <- 0.25
#' model1 <- get_boin(num_doses = 5, target = target)
#'
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' @references
#' Yan, F., Pan, H., Zhang, L., Liu, S., & Yuan, Y. (2019).
#' BOIN: An R Package for Designing Single-Agent and Drug-Combination
#' Dose-Finding Trials Using Bayesian Optimal Interval Designs.
#' Journal of Statistical Software, 27(November 2017), 0–35.
#' https://doi.org/10.18637/jss.v000.i00
#'
#' Liu, S., & Yuan, Y. (2015).
#' Bayesian optimal designs for Phase I clinical trials.
#' J. R. Stat. Soc. C, 64, 507–523.
#' https://doi.org/10.1111/rssc.12089
get_boin <- function(
    num_doses,
    target,
    use_stopping_rule = TRUE,
    stop_when_deescalation_impossible = FALSE,
    ...
) {

  x <- list(
    num_doses = num_doses,
    target = target,
    use_stopping_rule = use_stopping_rule,
    stop_when_deescalation_impossible = stop_when_deescalation_impossible,
    extra_args = list(...)
  )

  class(x) <- c('boin_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

#' @importFrom BOIN select.mtd get.boundary
#' @importFrom utils tail
#' @importFrom magrittr %>%
boin_selector <- function(outcomes, num_doses, target, use_stopping_rule,
                          stop_when_deescalation_impossible, ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  df_c <- model_frame_to_counts(df, num_doses = num_doses)
  x <- select.mtd(target = target, npts = df_c$n, ntox = df_c$tox)

  # Checks
  if(nrow(df) > 0) {
    if(max(df$dose) > num_doses) {
      stop("boin_selector - dose given exceeds the maximum.")
    }
  }

  if(nrow(df) == 0) {
    recommended_dose <- 1
    continue <- TRUE
  } else {
    last_dose <- df$dose %>% tail(1)
    n_d <- df_c$n[last_dose]
    tox_d <- df_c$tox[last_dose]
    bound <- get.boundary(target = target, ncohort = 1,
                          cohortsize = n_d + 1, ...)
    this_bound <- bound$full_boundary_tab[, n_d]
    if(tox_d <= this_bound['Escalate if # of DLT <=']) {
      # The trial can continue
      continue <- TRUE
      # Escalate, if possible
      recommended_dose <- pmin(num_doses, last_dose + 1)
      # But check recommended_dose is not eliminated, if using stopping rule
      n_r <- df_c$n[recommended_dose]
      if(use_stopping_rule & n_r > 0) {
        rec_bound <- get.boundary(target = target, ncohort = 1,
                                  cohortsize = n_r + 1, ...)
        this_rec_bound <- rec_bound$full_boundary_tab[, n_r]
        tox_r <- df_c$tox[recommended_dose]

        if( !is.na(this_rec_bound['Eliminate if # of DLT >=']) &
            tox_r >= this_rec_bound['Eliminate if # of DLT >=']) {
          # Do not escalate. Stay at last_dose
          recommended_dose <- last_dose
        }
      }

    } else if(tox_d >= this_bound['Deescalate if # of DLT >=']) {
      # De-escalate and possibly eliminate
      if(use_stopping_rule &
         !is.na(this_bound['Eliminate if # of DLT >=']) &
         tox_d >= this_bound['Eliminate if # of DLT >=']){

        # If elimination is path-dependent this approach could fail.

        if(last_dose == 1) {
          # Stop and recommend no dose.
          recommended_dose <- NA
          continue <- FALSE
        } else {
          # Eliminate this dose and all higher doses.
          # Select highest non-eliminated dose
          recommended_dose <- last_dose - 1
          continue <- TRUE
        }
      } else {
        # De-escalate, if possible
        # recommended_dose <- pmax(1, last_dose - 1)
        # continue <- TRUE

        recommended_dose <- last_dose - 1
        continue <- TRUE
        if(recommended_dose <= 0) {
          if(stop_when_deescalation_impossible) {
            recommended_dose <- NA
            continue <- FALSE
          } else {
            recommended_dose <- 1
          }
        }
      }
    } else {
      # As you were
      recommended_dose <- last_dose
      continue <- TRUE
    }
  }

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    num_doses = as.integer(num_doses),
    target = target,
    boin_fit = x,
    use_stopping_rule = use_stopping_rule,
    stop_when_deescalation_impossible = stop_when_deescalation_impossible,
    # ..+2 to stave off error in BOIN if cohortsize == 1:
    bound = get.boundary(target = target, ncohort = 1,
                         cohortsize = nrow(df) + 2, ...),
    df = df,
    df_c = df_c,
    recommended_dose = recommended_dose,
    continue = continue
  )

  class(l) = c('boin_selector', 'tox_selector', 'selector')
  l
}


# Factory interface

#' @export
fit.boin_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    target = selector_factory$target,
    use_stopping_rule = selector_factory$use_stopping_rule,
    stop_when_deescalation_impossible =
      selector_factory$stop_when_deescalation_impossible
  )
  args <- append(args, selector_factory$extra_args)
  do.call(boin_selector, args = args)
}

# Selector interface

#' @export
tox_target.boin_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.boin_selector <- function(x, ...) {
  return(length(x$df$dose))
}

#' @export
cohort.boin_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.boin_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.boin_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
num_doses.boin_selector <- function(x, ...) {
  return(x$num_doses)
}

#' @export
recommended_dose.boin_selector <- function(x, ...) {
  return(as.integer(x$recommended_dose))
}

#' @export
continue.boin_selector <- function(x, ...) {
  return(x$continue)
}

#' @export
tox_at_dose.boin_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
mean_prob_tox.boin_selector <- function(x, ...) {
  # The authors store prob(DLT) as an ordered variable with the probs as levels:
  # They also use '----' to show a dose has never been given.
  mean_s <- as.character(x$boin_fit$p_est$phat)
  mean_s[mean_s == '----'] <- NA
  return(as.numeric(mean_s))
}

#' @export
median_prob_tox.boin_selector <- function(x, ...) {
  prob_tox_quantile(x, p = 0.5, ...)
}

#' @export
dose_admissible.boin_selector <- function(x, ...) {
  if(x$use_stopping_rule) {
    n_d <- n_at_dose(x)
    t_d <- tox_at_dose(x)
    reject <- logical(length = num_doses(x))
    for(i in seq_along(reject)) {
      if(n_d[i] > 0) {
        this_bound <- x$bound$full_boundary_tab[, n_d[i]]
        boundary_t <- this_bound['Eliminate if # of DLT >=']
        if(is.na(boundary_t))
          reject[i] <- FALSE # Implicitly
        else
          reject[i] <- t_d[i] >= boundary_t # Explicitly
      } else {
        reject[i] <- FALSE # Implicitly
      }
    }
    # However, monotonic tox suggests doses higher than an inadmissible dose
    # are also inadmissible:
    cum_reject <- cumsum(reject) >= 1
    return(!cum_reject)
  } else {
    return(rep(TRUE, num_doses(x)))
  }
}

#' @export
prob_tox_quantile.boin_selector <- function(x, p,
  quantile_candidates = seq(0, 1, length.out = 101), ...) {

  reverse_engineer_prob_tox_quantile(x, p, quantile_candidates, ...)
}

#' @export
prob_tox_exceeds.boin_selector <- function(x, threshold, ...) {
  # The authors use beta-binomial conjugate approach. They use a
  # Beta(0.05, 0.05) prior but I could not find a justification for that.
  # They also use isotonic regression to ensure increasing estimates:
  pava_bb_prob_tox_exceeds(x, threshold, alpha = 0.05, beta = 0.05)
}

#' @export
supports_sampling.boin_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.boin_selector <- function(x, tall = FALSE, ...) {
  stop('boin_selector does not support sampling.')
}
