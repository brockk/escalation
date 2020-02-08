
#' Dose selector factory.
#'
#' @description
#' Along with \code{\link{selector}}, this is the second core class in the
#' \code{escalation} package. It exists to do one thing: fit outcomes from
#' dose-finding trials to the models we use to select doses.
#'
#' A \code{\link{selector_factory}} object is obtained by initially calling a
#' function like \code{\link{get_dfcrm}}, \code{\link{get_three_plus_three}} or
#' \code{\link{get_boin}}. Users may then add desired extra behaviour with
#' subsequent calls to functions like \code{\link{stop_when_n_at_dose}} or
#' \code{\link{stop_when_too_toxic}}.
#' \code{selector} objects are obtained by calling the \code{\link{fit}}
#' function on a \code{\link{selector_factory}} object. Refer to examples to see
#' how this works.
#'
#' @seealso \code{\link{selector}}
#'
#' @export
#'
#' @examples
#'
#' # Start with a simple CRM model
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#'
#' # Add a rule to stop when 9 patients are treated at the recommended dose
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_n_at_dose(n = 9, dose = 'recommended')
#'
#' # Add a rule to stop if toxicity rate at lowest dose likely exceeds target
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
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
#' # For contrast, let us consider a BOIN model on the same outcomes
#' boin_fitter <- get_boin(num_doses = length(skeleton), target = target)
#' fit4 <- boin_fitter %>% fit(outcomes)
#' fit4 %>% recommended_dose()
#' fit4 %>% continue()
selector_factory <- function() {
  # This function exists only to document the abstract class "selector_factory".
}
