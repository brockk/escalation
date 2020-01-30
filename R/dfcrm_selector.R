
get_dfcrm <- function(skeleton, target) {

  x <- list(
    skeleton = skeleton,
    target = target
  )
  class(x) <- c('selector_factory', 'dfcrm_selector_factory')
  return(x)
}

dfcrm_selector <- function(outcomes, skeleton, target) {

  df <- parse_phase1_outcomes(outcomes)
  x <- dfcrm::crm(prior = skeleton, target = target,
                  tox = df$tox, level = df$dose,
                  var.est = TRUE)

  l <- list(
    cohort = df$cohort,
    outcomes = outcomes,
    skeleton = skeleton,
    dfcrm_fit = x
  )

  class(l) = c('selector', 'dfcrm_selector')
  l
}

# Factory interface
fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {
  return(dfcrm_selector(outcomes,
                        selector_factory$skeleton,
                        selector_factory$target))
}

# Selector interface
num_patients.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level %>% length)
}

cohort.dfcrm_selector <- function(selector, ...) {
  return(selector$cohort)
}

doses_given.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$level)
}

tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$tox)
}

num_doses.dfcrm_selector <- function(selector, ...) {
  return(length(selector$skeleton))
}

recommended_dose.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$mtd)
}

continue.dfcrm_selector <- function(selector, ...) {
  return(TRUE)
}

n_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  purrr::map_int(dose_indices, ~ sum(selector %>% doses_given() == .x))
}

tox_at_dose.dfcrm_selector <- function(selector, ...) {
  dose_indices <- 1:(selector %>% num_doses())
  tox_seen <- selector %>% tox()
  purrr::map_int(dose_indices,
                 ~ sum(tox_seen[selector %>% doses_given() == .x])
  )
}

mean_prob_tox.dfcrm_selector <- function(selector, ...) {
  return(selector$dfcrm_fit$ptox)
}

median_prob_tox.dfcrm_selector <- function(selector, ...) {
  message('Note that dfcrm does not estimate median_prob_tox.')
  rep(NA, selector %>% num_doses())
}

prob_tox_exceeds.dfcrm_selector <- function(selector, threshold, iter = 1000,
                                            ...) {

  if(selector$dfcrm_fit$model == 'empiric') {
    # Sample beta from normal distribution with mean and stdev that match
    # posterior parameter estimates:
    beta <- rnorm(n = iter, mean = selector$dfcrm_fit$estimate,
                  sd = sqrt(selector$dfcrm_fit$post.var))
    # MAtrix with skeleton in each row
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

