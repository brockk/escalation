
#' Dose selector.
#'
#' @description
#' This is a core class in this package. It encapsulates that an object (e.g. a
#' CRM model) is able to recommend doses, keep track of how many patients have
#' been treated at what doses, what toxicity outcomes have been seen, and
#' whether a trial should continue. It offers a consistent interface to
#' dose-finding methods from several packages, including \code{dfcrm},
#' \code{bcrm}, \code{trialr}, etc (TODO).
#'
#' Once you have a standardised interface, modularisation offers a powerful way
#' to adorn dose-finding methods with extra desirable behaviour. \code{selector}
#' objects can be daisy-chained togther using \code{magrittr}'s pipe operator.
#' For instance, the CRM fitting method in \code{dfcrm} is fantastic because it
#' runs quickly and is simple to call. However, it does not recommend that a
#' trial stops if a dose is too toxic or if n patients have already been treated
#' at the recommended dose. Each of these behaviours can be bolted on.
#' Furthermore, those behaviours and more can be bolted on to any dose selector
#' because of the modular approach implemented in \code{dosefinding}.
#' See Examples.
#'
#' The \code{selector} class also supports that an object will be able to
#' perform inferential calculations on the rates of toxicity, although naturally
#' the sophistication of those calculations will vary by model implementation.
#'
#' @export
#'
#' @examples
#'
#' # Start with a simple CRM model
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton, target)
#'
#' # Add a rule to stop when 9 patients are treated at the recommended dose
#' model2 <- get_dfcrm(skeleton, target) %>%
#'   stop_when_n_at_dose(n = 9, dose = 'recommended')
#'
#' # Add a rule to stop if toxicity rate at lowest dose likely exceeds target
#' model3 <- get_dfcrm(skeleton, target) %>%
#'   stop_when_n_at_dose(n = 9, dose = 'recommended') %>%
#'   stop_when_too_toxic(dose = 1, tox_threshold = target, confidence = 0.5)
#'
#' # We now have three CRM models that differ in their stopping behaviour.
#' # Let's fit each to some outcomes to see those differences:
#'
#' outcomes <- '1NNN 2NTT 1NNT'
#' fit1 <- model1 %>% fit(outcomes)
#' fit2 <- model2 %>% fit(outcomes)
#' fit3 <- model3 %>% fit(outcomes)
#'
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' fit3 %>% recommended_dose()
#' fit3 %>% continue()
#' # Already model3 wants to stop because of excessive toxicity.
#'
#' # Let's carry on with models 1 and 2 by adding another cohort:
#'
#' outcomes <- '1NNN 2NTT 1NNT 1NNN'
#' fit1 <- model1 %>% fit(outcomes)
#' fit2 <- model2 %>% fit(outcomes)
#'
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' # Model1 wants to continue - in fact it will never stop.
#' # In contrast, model2 has seen 9 at dose 1 so, rather than suggest dose 1
#' # again, it suggests the trial should stop.
#'
#' # TODO non-CRM examples
selector <- function() {
  # This function exists merely to document the abstract class "selector".
}

#' @export
model_frame.selector <- function(selector, ...) {
  tibble::tibble(
    patient = seq(1, selector %>% num_patients()),
    cohort = selector %>% cohort(),
    dose = selector %>% doses_given(),
    tox = selector %>% tox()
  )
}

#' @export
empiric_tox_rate.selector <- function(selector, ...) {
  return(selector %>% tox_at_dose() / selector %>% n_at_dose())
}
