
#' Stop when there are n patients in total.
#'
#' @description
#' This function adds a restriction to stop a trial when n patients have been
#' evaluated. It does this by adding together the number of patients treated at
#' all doses and stopping when that total exceeds n.
#'
#' Dose selectors are designed to be daisy-chained together to achieve different
#' behaviours. This class is a **greedy** selector, meaning that it prioritises
#' its own behaviour over the behaviour of other selectors in the chain. That
#' is, it will advocate stopping when the condition has been met, even if the
#' selectors further up the chain would advocate to keep going. In can be
#' interpreted as an overriding selector. This allows the decision to stop to
#' be executed as soon as it is warranted. Be aware though, that there are other
#' selectors that can be placed after this class that will override the stopping
#' behaviour. See Examples.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param n Stop when there are this many patients.
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
#' # Create CRM model that will stop when 15 patients are evaluated:
#' model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 15)
#'
#' # With 12 patients, this trial should not stop:
#' fit1 <- model1 %>% fit('1NNN 2NTN 2TNN 2NNN')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' # With 15 patients, this trial should stop:
#' fit2 <- model1 %>% fit('1NNN 2NTN 2TNN 2NNN 2NTT')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' # The stopping behaviour can be overruled by the order of selectors.
#' # In model2, demanding 9 at recommended dose will trump stopping at 12:
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   demand_n_at_dose(dose = 'recommended', n = 9)
#'
#' # In model3, stopping at 12 will trump demanding 9 at recommended dose:
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   demand_n_at_dose(dose = 'recommended', n = 9) %>%
#'   stop_at_n(n = 12)
#'
#' # This model will continue because 9 have not been seen at recommended dose.
#' fit3 <- model2 %>% fit('1NNN 2NNN 2NNN 3NNN')
#' fit3 %>% recommended_dose()
#' fit3 %>% continue()
#'
#' # This model will stop because 12 have been seen.
#' fit4 <- model3 %>% fit('1NNN 2NNN 2NNN 3NNN')
#' fit4 %>% recommended_dose()
#' fit4 %>% continue()
#'
#' # With enough observations though, both models will advise stopping because
#' # both conditions have been met:
#' fit5 <- model2 %>% fit('1NNN 2NNN 2NNN 5NNN 5NNN 5NNN')
#' fit5 %>% recommended_dose()
#' fit5 %>% continue()
#'
#' fit6 <- model3 %>% fit('1NNN 2NNN 2NNN 5NNN 5NNN 5NNN')
#' fit6 %>% recommended_dose()
#' fit6 %>% continue()
#'
stop_at_n <- function(parent_selector_factory, n) {

  x <- list(
    parent = parent_selector_factory,
    n = n
  )
  class(x) <- c('stop_at_n_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

stop_at_n_selector <- function(parent_selector, n) {

  l <- list(
    parent = parent_selector,
    n = n
  )

  class(l) = c('stop_at_n_selector', 'derived_dose_selector', 'selector')
  l
}

# Factory interface

#' @export
fit.stop_at_n_selector_factory <- function(selector_factory, outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(stop_at_n_selector(parent_selector = parent_selector,
                            n = selector_factory$n))
}

# Selector interface

#' @export
continue.stop_at_n_selector <- function(selector, ...) {
  parent_continue <- selector$parent %>% continue()
  this_continue <- selector %>% num_patients() < selector$n
  return(parent_continue & this_continue)
}
