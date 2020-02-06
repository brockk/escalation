
#' Stop when a dose is too toxic.
#'
#' This method stops a dose-finding trial when sufficient probabilistic
#' confidence is reached that the rate of toxicity at a dose exceeds some
#' threshold. In other words, it stops when it is likely that a dose is too
#' toxic. It can stop when the rule is triggered at the recommended dose, at a
#' particular dose, or at any dose. See Details.
#'
#' The method for calculating probability mass for toxicity rates will
#' ultimately be determined by the dose-finding model used and the attendant
#' inferential mechanism. For instance, the \code{\link[dfcrm]{crm}} function in
#' the dfcrm package calculates the posterior expected mean and variance of the
#' slope parameter in a CRM model. It does not use MCMC to draw samples from the
#' posterior distribution. Thus, to perform inference on the posterior
#' probability of toxicity, this package assumes the dfcrm slope parameter
#' follows a normal distribution with the mean and variance calculated by dfcrm.
#' In contrast, the \code{\link[trialr]{stan_crm}} function in the trialr
#' package needs no such assumption because it samples from the posterior
#' parameter distribution and uses those samples to infer on the posterior
#' probability of toxicity at each dose, dependent on the chosen model for the
#' dose-toxicity curve.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param dose \code{'any'} to stop when any dose is too toxic;
#' \code{'recommended'} to stop when the recommended dose is too toxic; or an
#' integer to stop when a particular dose-level is too toxic.
#' @param tox_threshold We ae interested in toxicity probabilities greater than
#' this threshold.
#' @param confidence Stop when there is this much total probability mass
#' supporting that the toxicity rate exceeds the threshold.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#'
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_too_toxic(dose = 'any', tox_threshold = 0.5, confidence = 0.7)
#'
#' outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
#'
#' fit1 <- model1 %>% fit(outcomes)
#' fit2 <- model2 %>% fit(outcomes)
#'
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#' fit2 %>% prob_tox_exceeds(0.5)
stop_when_too_toxic <- function(parent_selector_factory, dose, tox_threshold,
                                confidence) {

  x <- list(
    parent = parent_selector_factory,
    dose = dose,
    tox_threshold = tox_threshold,
    confidence = confidence
  )
  class(x) <- c('selector_factory',
                'derived_dose_selector_factory',
                'stop_when_too_toxic_selector_factory')
  return(x)
}

stop_when_too_toxic_selector <- function(parent_selector, dose, tox_threshold,
                                         confidence) {

  l <- list(
    parent = parent_selector,
    dose = dose,
    tox_threshold = tox_threshold,
    confidence = confidence
  )

  class(l) = c('selector', 'derived_dose_selector',
               'stop_when_too_toxic_selector')
  l
}

# Factory interface

#' @export
fit.stop_when_too_toxic_selector_factory <- function(selector_factory, outcomes,
                                                     ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(stop_when_too_toxic_selector(
    parent_selector = parent_selector,
    dose = selector_factory$dose,
    tox_threshold = selector_factory$tox_threshold,
    confidence = selector_factory$confidence
    )
  )
}

# Selector interface

#' @export
recommended_dose.stop_when_too_toxic_selector <- function(selector, ...) {
  prob_too_tox <- selector %>% prob_tox_exceeds(selector$tox_threshold)
  if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
    if(prob_too_tox[selector$dose] >= selector$confidence) {
      return(NA)
    }
  }

  # By default:
  return(selector$parent %>% recommended_dose())
}

#' @export
continue.stop_when_too_toxic_selector <- function(selector, ...) {
  prob_too_tox <- selector %>% prob_tox_exceeds(selector$tox_threshold)
  if(selector$dose == 'any') {
    if(any(prob_too_tox >= selector$confidence)) {
      return(FALSE)
    }
  } else if(selector$dose == 'recommended') {
    rec_dose <- selector %>% recommended_dose()
    if(prob_too_tox[rec_dose] >= selector$confidence) {
      return(FALSE)
    }
  }
  else if(selector$dose >= 1 & selector$dose <= selector %>% num_doses()) {
    if(prob_too_tox[selector$dose] >= selector$confidence) {
      return(FALSE)
    }
  }

  # By default:
  return(selector$parent %>% continue())
}
