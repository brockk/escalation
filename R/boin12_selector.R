
#' Get an object to fit the BOIN12 model for phase I/II dose-finding.
#'
#' @description
#' This function returns an object that can be used to fit the BOIN12 model for
#' phase I/II dose-finding, i.e. it selects doses according to efficacy and
#' toxicity outcomes.
#'
#' @param num_doses integer, num of doses under investigation
#' @param phi_t Probability of toxicity threshold
#' @param phi_e Probability of efficacy threshold
#' @param u1 utility of efficacy without toxicity, 100 by default
#' @param u2 utility of no efficacy and no toxicity, between u1 and u4
#' @param u3 utility of efficacy and toxicity, between u1 and u4
#' @param u4 utility of toxicity without efficacy , 0 by default
#' @param n_star when tox is within bounds, stop exploring higher doses when n
#'   at dose is greater than or equal to this value. 6 by default.
#' @param c_t certainty required to flag excess toxicity, 0.95 by default
#' @param c_e certainty required to flag deficient efficacy, 0.9 by default
#' @param start_dose index of starting dose, 1 by default (i.e. lowest dose)
#' @param prior_alpha first shape param for prior on beta prior, 1 by default
#' @param prior_beta second shape param for prior on beta prior, 1 by default
#' @param ... Extra args are passed onwards.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' BOIN12 model to outcomes.
#'
#' @export
#'
#' @examples
#' # Examples in Lin et al.
#' model <- get_boin12(num_doses = 5, phi_t = 0.35, phi_e = 0.25,
#'                     u2 = 40, u3 = 60, n_star = 6)
#' fit <- model %>% fit('1NNN 2ENT 3ETT 2EEN')
#' fit %>% recommended_dose()
#' fit %>% continue()
#' fit %>% is_randomising()
#' fit %>% dose_admissible()
#' fit %>% prob_administer()
#'
#' @references
#' Lin, R., Zhou, Y., Yan, F., Li, D., & Yuan, Y. (2020).
#' BOIN12: Bayesian optimal interval phase I/II trial design for utility-based
#' dose finding in immunotherapy and targeted therapies.
#' JCO precision oncology, 4, 1393-1402.
get_boin12 <- function(
  num_doses,
  phi_t, phi_e,
  u1 = 100, u2, u3, u4 = 0,
  n_star = 6,
  c_t = 0.95, c_e = 0.9,
  start_dose = 1,
  prior_alpha = 1, prior_beta = 1,
  ...) {

  x <- list(
    num_doses = num_doses,
    phi_t = phi_t,
    phi_e = phi_e,
    u1 = u1,
    u2 = u2,
    u3 = u3,
    u4 = u4,
    n_star = n_star,
    c_t = c_t,
    c_e = c_e,
    start_dose = start_dose,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    extra_args = list(...)
  )

  class(x) <- c('boin12_selector_factory',
                'eff_tox_selector_factory',
                'selector_factory')
  return(x)
}


#' @importFrom magrittr %>%
#' @importFrom dplyr rename
boin12_selector <- function(
    outcomes, num_doses,
    phi_t, phi_e,
    u1 = 100, u2, u3, u4 = 0, n_star,
    c_t = 0.95, c_e = 0.9,
    start_dose = 1,
    prior_alpha = 1, prior_beta = 1,
    ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_2_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  df_c <- model_frame_to_counts(df, num_doses = num_doses)

  # Checks
  if(u2 < u4 | u2 > u1)
    stop(paste("u2 should be between", u4, "and", u1))
  if(u3 < u4 | u3 > u1)
    stop(paste("u3 should be between", u4, "and", u1))

  # Define recommended_dose and continue
  dose <- tox <- eff <- NULL
  df_for_ms_funcs <- df %>%
    rename(
      Dose = dose,
      Toxicity = tox,
      Efficacy = eff
    )

  x <- boin12_next_dose(
    data = df_for_ms_funcs,
    ndoses = num_doses,
    maxN = Inf, # Disable this
    maxN_dose = Inf, # Disable this
    start = start_dose,
    phi_t = phi_t,
    phi_e = phi_e,
    Nstar = n_star,
    c_t = c_t,
    c_e = c_e,
    u1 = u1,
    u2 = u2,
    u3 = u3,
    u4 = u4,
    alpha = prior_alpha,
    beta = prior_beta,
    verbose = FALSE
  )
  recommended_dose <- x$next_dose
  admissible <- x$admissible
  continue <- TRUE
  l <- list(
    outcomes = outcomes,
    num_doses = as.integer(num_doses),
    df = df,
    df_c = df_c,
    recommended_dose = recommended_dose,
    admissible = admissible,
    continue = continue,
    phi_t = phi_t,
    phi_e = phi_e,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta
  )

  class(l) = c('boin12_selector', 'eff_tox_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.boin12_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    phi_t = selector_factory$phi_t,
    phi_e = selector_factory$phi_e,
    u1 = selector_factory$u1,
    u2 = selector_factory$u2,
    u3 = selector_factory$u3,
    u4 = selector_factory$u4,
    n_star = selector_factory$n_star,
    c_t = selector_factory$c_t,
    c_e = selector_factory$c_e,
    start_dose = selector_factory$start_dose,
    prior_alpha = selector_factory$prior_alpha,
    prior_beta = selector_factory$prior_beta
  )
  args <- append(args, selector_factory$extra_args)
  do.call(boin12_selector, args = args)
}



# Selector interface

#' @export
tox_limit.boin12_selector <- function(x, ...) {
  return(x$phi_t)
}

#' @export
eff_limit.boin12_selector <- function(x, ...) {
  return(x$phi_e)
}

#' @export
num_patients.boin12_selector <- function(x, ...) {
  return(length(x$df$dose))
}

#' @export
cohort.boin12_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.boin12_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.boin12_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
eff.boin12_selector <- function(x, ...) {
  return(x$df$eff)
}

#' @export
num_doses.boin12_selector <- function(x, ...) {
  return(x$num_doses)
}

#' @export
recommended_dose.boin12_selector <- function(x, ...) {
  return(as.integer(x$recommended_dose))
}

#' @export
continue.boin12_selector <- function(x, ...) {
  return(x$continue)
}

#' @export
tox_at_dose.boin12_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
eff_at_dose.boin12_selector <- function(x, ...) {
  return(x$df_c$eff)
}

#' @export
mean_prob_tox.boin12_selector <- function(x, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + tox_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - tox_at_dose(x)
  return(alpha / (alpha + beta))
}

#' @export
median_prob_tox.boin12_selector <- function(x, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + tox_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - tox_at_dose(x)
  return((alpha - 1 / 3) / (alpha + beta - 2 / 3))
}

#' @export
mean_prob_eff.boin12_selector <- function(x, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + eff_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - eff_at_dose(x)
  return(alpha / (alpha + beta))
}

#' @export
median_prob_eff.boin12_selector <- function(x, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + eff_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - eff_at_dose(x)
  return((alpha - 1 / 3) / (alpha + beta - 2 / 3))
}

#' @export
dose_admissible.boin12_selector <- function(x, ...) {
  z <- seq(1, num_doses(x)) %in% x$admissible
  return(z)
}

#' @export
prob_tox_quantile.boin12_selector <- function(x, p, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + tox_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - tox_at_dose(x)
  return(qbeta(p = p, shape1 = alpha, shape2 = beta, lower.tail = TRUE))
}

#' @importFrom stats qbeta
#' @export
prob_tox_exceeds.boin12_selector <- function(x, threshold, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + tox_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - tox_at_dose(x)
  return(
    pbeta(q = threshold, shape1 = alpha, shape2 = beta, lower.tail = FALSE)
  )
}

#' @importFrom stats qbeta
#' @export
prob_eff_quantile.boin12_selector <- function(x, p, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + eff_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - eff_at_dose(x)
  return(qbeta(p = p, shape1 = alpha, shape2 = beta, lower.tail = TRUE))
}

#' @export
prob_eff_exceeds.boin12_selector <- function(x, threshold, ...) {
  # The authors use Beta priors on Pr(Tox) and Pr(Eff) parameters to infer
  # whether doses are sufficiently tolerable and active. Reuse that distribution
  alpha <- x$prior_alpha + eff_at_dose(x)
  beta <- x$prior_beta + n_at_dose(x) - eff_at_dose(x)
  return(
    pbeta(q = threshold, shape1 = alpha, shape2 = beta, lower.tail = FALSE)
  )
}

#' @export
supports_sampling.boin12_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.boin12_selector <- function(x, tall = FALSE, ...) {
  stop('boin12_selector does not support sampling.')
}

#' @export
prob_eff_samples.boin12_selector <- function(x, tall = FALSE, ...) {
  stop('boin12_selector does not support sampling.')
}
