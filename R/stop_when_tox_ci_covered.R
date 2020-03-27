
#' Stop when uncertainty interval of prob tox is covered.
#'
#' This method stops a dose-finding trial when the symmetric uncertainty
#' interval for the probability of toxicity falls within a range. This allows
#' trials to be stopped when sufficient precision on the pobability of toxicity
#' has been achieved. See Details.
#'
#' The method for calculating probability mass for toxicity rates will
#' ultimately be determined by the dose-finding model used and the attendant
#' inferential mechanism. For instance, the \code{\link[dfcrm]{crm}} function in
#' the dfcrm package calculates the posterior expected mean and variance of the
#' slope parameter in a CRM model. It does not use MCMC to draw samples from the
#' posterior distribution. Thus, to perform inference on the posterior
#' probability of toxicity, this package assumes the dfcrm slope parameter
#' follows a normal distribution with the mean and variance calculated by dfcrm.
#' In contrast, the \code{stan_crm} function in the \code{trialr}
#' package needs no such assumption because it samples from the posterior
#' parameter distribution and uses those samples to infer on the posterior
#' probability of toxicity at each dose, dependent on the chosen model for the
#' dose-toxicity curve.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param dose \code{'any'} to stop when the interval for any dose is covered;
#' \code{'recommended'} to stop when the interval for the recommended dose is
#' covered ; or an integer to stop when the interval for a particular dose-level
#' is covered.
#' @param lower Stop when lower interval bound exceeds this value
#' @param upper Stop when upper interval bound is less than this value
#' @param width Width of the uncertainty interval. Default is 0.9, i.e. a range
#' from the 5th to the 95th percentiles.
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
#' # We compare a CRM model without this stopping rule:
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#' # To two with it, the first demanding a relatively tight CI:
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_tox_ci_covered(dose = 'recommended', lower = 0.15, upper = 0.35)
#' # and the second demanding a relatively loose CI:
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_tox_ci_covered(dose = 'recommended', lower = 0.05, upper = 0.45)
#'
#' outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
#' fit1 <- model1 %>% fit(outcomes)
#' fit2 <- model2 %>% fit(outcomes)
#' fit3 <- model3 %>% fit(outcomes)
#'
#' # Naturally the first does not advocate stopping:
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' # The second does not advocate stopping either:
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#' # This is because the CI is too wide:
#' fit2 %>% prob_tox_quantile(p = 0.05)
#' fit2 %>% prob_tox_quantile(p = 0.95)
#'
#' # However, the third design advocates stopping because the CI at the
#' # recommended dose is covered:
#' fit3 %>% recommended_dose()
#' fit3 %>% continue()
#' # To verify the veracity, inspect the quantiles:
#' fit3 %>% prob_tox_quantile(p = 0.05)
#' fit3 %>% prob_tox_quantile(p = 0.95)
stop_when_tox_ci_covered <- function(parent_selector_factory, dose, lower,
                                     upper, width = 0.9) {

  x <- list(
    parent = parent_selector_factory,
    dose = dose,
    lower = lower,
    upper = upper,
    width = width
  )
  class(x) <- c('stop_when_tox_ci_covered_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

stop_when_tox_ci_covered_selector <- function(parent_selector, dose, lower,
                                              upper, width = 0.9) {

  l <- list(
    parent = parent_selector,
    dose = dose,
    lower = lower,
    upper = upper,
    width = width
  )

  class(l) = c('stop_when_tox_ci_covered_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @export
fit.stop_when_tox_ci_covered_selector_factory <- function(
  selector_factory, outcomes, ...) {

  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(stop_when_tox_ci_covered_selector(
    parent_selector = parent_selector,
    dose = selector_factory$dose,
    lower = selector_factory$lower,
    upper = selector_factory$upper,
    width = selector_factory$width
  ))
}

# Selector interface

#' @export
continue.stop_when_tox_ci_covered_selector <- function(x, ...) {

  # Stop now if parent wants:
  if(!x$parent %>% continue()) return(FALSE)

  lower_bounds <- prob_tox_quantile(x, p = (1 - x$width) / 2)
  upper_bounds <- prob_tox_quantile(x, p =  1 - (1 - x$width) / 2)

  if(x$dose == 'any') {
    if(any(lower_bounds >= x$lower & upper_bounds <= x$upper,
           na.rm = TRUE)) {
      return(FALSE)
    }
  } else if(x$dose == 'recommended') {
    rec_dose <- x %>% recommended_dose()
    if(!is.na(lower_bounds[rec_dose]) &
       lower_bounds[rec_dose] >= x$lower &
       !is.na(upper_bounds[rec_dose]) &
       upper_bounds[rec_dose] <= x$upper) {
      return(FALSE)
    }
  } else if(x$dose >= 1 & x$dose <= x %>% num_doses()) {
    if(!is.na(lower_bounds[x$dose]) &
       lower_bounds[x$dose] >= x$lower &
       !is.na(upper_bounds[x$dose]) &
       upper_bounds[x$dose] <= x$upper) {
      return(FALSE)
    }
  }

  # By default:
  return(x$parent %>% continue())
}
