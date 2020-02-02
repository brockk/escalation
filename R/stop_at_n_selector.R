
#' Stop when there are n patients in total.
#'
#' This method adds together the number of patients treated at all doses and
#' stops a dose-finding trial when there are at least n patients in total.
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
#' model1 <- get_dfcrm(skeleton, target) %>%
#'   stop_at_n(n = 15)
#'
#' fit1 <- model1 %>% fit('1NNN 2NTN 2TNN 2NNN')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#' fit2 <- model1 %>% fit('1NNN 2NTN 2TNN 2NNN 2NTT')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
stop_at_n <- function(parent_selector_factory, n) {

  x <- list(
    parent = parent_selector_factory,
    n = n
  )
  class(x) <- c('selector_factory',
                'derived_dose_selector_factory',
                'stop_at_n_selector_factory')
  return(x)
}

stop_at_n_selector <- function(parent_selector, n) {

  l <- list(
    parent = parent_selector,
    n = n
  )

  class(l) = c('selector', 'derived_dose_selector', 'stop_at_n_selector')
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
