
#' Select dose by the CIBP selection criterion.
#'
#' This method selects dose by the convex infinite bounds penalisation (CIBP)
#' criterion of Mozgunov & Jaki. Their method is mindful of the uncertainty in
#' the estimates of the probability of toxicity and uses an asymmetry parameter
#' to penalise escalation to risky doses.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param a Number between 0 and 2, the asymmetry parameter. See References.
#' @param target We seek a dose with this probability of toxicity. If not
#' provided, the value will be sought from the parent dose-selector.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.33
#'
#' # Let's compare escalation behaviour of a CRM model without CIBP criterion:
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#' # To one with the CIBP criterion:
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   select_dose_by_cibp(a = 0.3)
#'
#' # Despite one-in-three tox at first dose, regular model is ready to escalate:
#' model1 %>% fit('1NTN') %>% recommended_dose()
#' # But the model using CIBP is more risk averse:
#' model2 %>% fit('1NTN') %>% recommended_dose()
#'
#' @references
#' Mozgunov P, Jaki T. Improving safety of the continual reassessment method via
#' a modified allocation rule. Statistics in Medicine.1-17. doi:10.1002/sim.8450
select_dose_by_cibp <- function(parent_selector_factory, a, target = NULL) {

  x <- list(
    parent = parent_selector_factory,
    a = a,
    target = target
  )
  class(x) <- c('cibp_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

cibp_dose_selector <- function(parent_selector, a, target = NULL) {

  l <- list(
    parent = parent_selector,
    a = a,
    target = target
  )

  class(l) = c('cibp_dose_selector', 'derived_dose_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.cibp_dose_selector_factory <- function(selector_factory, outcomes, ...) {
  parent_selector <- selector_factory$parent %>% fit(outcomes, ...)
  return(cibp_dose_selector(parent_selector = parent_selector,
                            a = selector_factory$a,
                            target = selector_factory$target))
}

# Selector interface

#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @export
recommended_dose.cibp_dose_selector <- function(x, ...) {
  parent_rec_d <- recommended_dose(x$parent)
  if(is.null(x$target)) {
    target <- tox_target(x$parent)
  } else {
    target <- x$target
  }
  if(is.null(target)) {
    stop('Target toxicity probability is required when selecting dose by CIBP')
  }
  a <- x$a

  if(num_patients(x) == 0) {
    # No dose given, so just go with whatever parent proposes
    return(parent_rec_d)
  } else {
    .draw <- NULL
    prob_tox <- x$parent %>% prob_tox_samples() %>% select(-.draw)
    numerator <- (prob_tox - target)^2
    denominator <- (prob_tox^a) * (1 - prob_tox)^(2 - a)
    which.min(colMeans(numerator / denominator)) %>% unname()
  }
}

#' @export
print.cibp_dose_selector <- function(x, ...) {
  .dose_selector_print(x, ...)
}

#' @export
as_tibble.cibp_dose_selector <- function(x, ...) {
  .dose_selector_to_tibble(x, ...)
}

#' @export
summary.cibp_dose_selector <- function(object, ...) {
  .dose_selector_summary(object, ...)
}
