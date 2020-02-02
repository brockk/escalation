
#' Get an object to fit the CRM model using the dfcrm package.
#'
#' @param skeleton Dose-toxicity skeleton, a non-decreasing vector of
#' probabilities.
#' @param target We seek a dose with this probability of toxicity.
#' @param ... Extra args are passed to \code{\link[dfcrm]{crm}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' CRM model to outcomes.
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
#' Ken Cheung (2019). dfcrm: Dose-Finding by the Continual Reassessment Method.
#' R package version 0.2-2.1. https://CRAN.R-project.org/package=dfcrm
#'
#' O’Quigley J, Pepe M, Fisher L. Continual reassessment method: a practical
#' design for phase 1 clinical trials in cancer. Biometrics. 1990;46(1):33-48.
#' doi:10.2307/2531628
get_dfcrm <- function(skeleton, target, ...) {

  x <- list(
    skeleton = skeleton,
    target = target,
    extra_args = list(...)
  )

  class(x) <- c('selector_factory',
                'tox_selector_factory',
                'dfcrm_selector_factory')
  return(x)
}

dfcrm_selector <- function(outcomes, skeleton, target, ...) {

  df <- parse_phase1_outcomes(outcomes)
  x <- dfcrm::crm(prior = skeleton, target = target,
                  tox = df$tox, level = df$dose,
                  var.est = TRUE, ...)

  # Checks
  if(max(df$dose) > length(skeleton)) {
    stop('dfcrm_selector - maximum dose given exceeds number of doses.')
  }

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    skeleton = skeleton,
    dfcrm_fit = x
  )

  class(l) = c('selector', 'tox_selector', 'dfcrm_selector')
  l
}

# Factory interface

#' @export
fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {

  args <- list(
    outcomes = outcomes,
    skeleton = selector_factory$skeleton,
    target = selector_factory$target
  )
  args <- append(args, selector_factory$extra_args)
  do.call(dfcrm_selector, args = args)
}

# Selector interface

#' @importFrom magrittr %>%
#' @export
num_patients.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level %>% length)
}

#' @export
cohort.dfcrm_selector <- function(selector, ...) {
  return(selector$cohort)
}

#' @export
doses_given.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level)
}

#' @export
tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$tox)
}

#' @export
num_doses.dfcrm_selector <- function(selector, ...) {
  return(length(selector$skeleton))
}

#' @export
recommended_dose.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$mtd)
}

#' @export
continue.dfcrm_selector <- function(selector, ...) {
  return(TRUE)
}

#' @importFrom magrittr %>%
#' @importFrom purrr map_int
#' @export
n_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  map_int(dose_indices, ~ sum(selector %>% doses_given() == .x))
}

#' @importFrom magrittr %>%
#' @importFrom purrr map_int
#' @export
tox_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  tox_seen <- selector %>% tox()
  map_int(dose_indices,
                 ~ sum(tox_seen[selector %>% doses_given() == .x])
  )
}

#' @export
mean_prob_tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$ptox)
}

#' @export
median_prob_tox.dfcrm_selector <- function(selector, ...) {
  message('Note that dfcrm does not estimate median_prob_tox.')
  rep(NA, selector %>% num_doses())
}

#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @export
prob_tox_exceeds.dfcrm_selector <- function(selector, threshold, iter = 1000,
                                            ...) {

  if(selector$dfcrm_fit$model == 'empiric') {
    # Sample beta from normal distribution with mean and stdev that match
    # posterior parameter estimates:
    beta <- rnorm(n = iter, mean = selector$dfcrm_fit$estimate,
                  sd = sqrt(selector$dfcrm_fit$post.var))
    # Matrix with skeleton in each row
    skeleton_matrix <- matrix(selector$skeleton, nrow = iter,
                              ncol = selector %>% num_doses, byrow = TRUE)
    # Raise each row to one of the sampled beta values:
    prob_tox_sample <- skeleton_matrix ^ exp(beta)
    # Prob(Prob(Tox) > threshold) is approximated by:
    colMeans(prob_tox_sample > threshold)
  } else if(selector$dfcrm_fit$model == 'logistic') {
    # dfcrm fixes the intercept value:
    alpha <- selector$dfcrm_fit$intcpt
    # Sample beta from normal distribution with mean and stdev that match
    # posterior parameter estimates:
    beta <- rnorm(n = iter, mean = selector$dfcrm_fit$estimate,
                  sd = sqrt(selector$dfcrm_fit$post.var))
    stop('TODO - code not finished yet')
  } else {
    stop(paste0("Don't know what to do with dfcrm model '",
                selector$dfcrm_fit$model, "'"))
  }
}
