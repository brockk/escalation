
#' Get an object to fit the CRM model using the dfcrm package.
#'
#' @param num_doses Number of doses under investigation.
#' @param target We seek a dose with this probability of toxicity.
#' @param ... Extra args are passed to \code{\link[BOIN]{select.mtd}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' BOIN model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton, target)
#'
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' model2 <- get_dfcrm(skeleton, target, model = 'logistic')
#' model2 %>% fit(outcomes) %>% recommended_dose()
#'
#' @references
#' TODO
get_boin <- function(num_doses, target, ...) {

  x <- list(
    num_doses = num_doses,
    target = target,
    extra_args = list(...)
  )

  class(x) <- c('selector_factory', 'boin_selector_factory')
  return(x)
}

boin_selector <- function(outcomes, num_doses, target, ...) {

  df <- parse_phase1_outcomes(outcomes)
  # TODO stop if df$dose > num_doses
  df_c <- phase1_outcomes_to_counts(outcomes = outcomes, num_doses = num_doses)

  x <- BOIN::select.mtd(target = target, npts = df_c$n, ntox = df_c$tox, ...)

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    num_doses = num_doses,
    target = target,
    boin_fit = x,
    df = df,
    df_c = df_c
  )

  class(l) = c('selector', 'boin_selector')
  l
}

# Factory interface

#' @export
fit.boin_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    num_doses = selector_factory$num_doses,
    target = selector_factory$target
  )
  args <- append(args, selector_factory$extra_args)
  do.call(boin_selector, args = args)
}

# Selector interface

#' @export
num_patients.boin_selector <- function(selector, ...) {
  return(selector$df$num_patients)
}

#' @export
cohort.boin_selector <- function(selector, ...) {
  return(selector$df$cohort)
}

#' @export
doses_given.boin_selector <- function(selector, ...) {
  return(selector$df$dose)
}

#' @export
tox.boin_selector <- function(selector, ...) {
  return(selector$df$tox)
}

#' @export
num_doses.boin_selector <- function(selector, ...) {
  return(selector$num_doses)
}

#' @export
recommended_dose.boin_selector <- function(selector, ...) {
  last_dose <- selector$df$dose %>% tail(1)
  if(length(last_dose) == 0) last_dose <- 1
  n_d <- n_at_dose(selector)[last_dose]
  tox_d <- tox_at_dose(selector)[last_dose]
  bound <- BOIN::get.boundary(target = selector$target,
                              ncohort = 1, cohortsize = n_d)
  this_bound <- bound$full_boundary_tab[, n_d]
  if(tox_d <= this_bound['Escalate if # of DLT <=']) {
    # Escalate if possible
    return(pmin(selector$num_doses, last_dose + 1))
  } else if(tox_d >= this_bound['Deescalate if # of DLT >=']) {
    # De-escalate if possible
    # TODO what to do if cannot de-escalate?
    return(pmax(1, last_dose - 1))
    # TODO what about stopping and elimination?
  } else {
    # Remain
    return(last_dose)
  }
}

#' @export
continue.boin_selector <- function(selector, ...) {
  # TODO BOIN provides stopping rules natively. How to implement those?
  # Perhaps creation level parameter use_stopping_rules that defaults to TRUE
  return(TRUE)
}

#' @export
n_at_dose.boin_selector <- function(selector, ...) {
  return(selector$df_c$n)
}

#' @export
tox_at_dose.boin_selector <- function(selector, ...) {
  return(selector$df_c$tox)
}

#' @export
mean_prob_tox.boin_selector <- function(selector, ...) {
  # The authors store prob(DLT) as an ordered variable with the probs as levels:
  # They also use '----' to show a dose has never been given.
  mean_s <- as.character(selector$boin_fit$p_est$phat)
  mean_s[mean_s == '----'] <- NA
  return(as.numeric(mean_s))
}

#' @export
median_prob_tox.boin_selector <- function(selector, ...) {
  message('Note that BOIN does not estimate median_prob_tox.')
  rep(NA, selector %>% num_doses())
}

#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @export
prob_tox_exceeds.boin_selector <- function(selector, threshold, iter = 1000,
                                            ...) {
  # TODO - use beta-binomial conjugate approach
  return(rep(NA, selector %>% num_doses()))
}
