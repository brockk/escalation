
#' Stay at the current dose when num_tox of num_patients have experienced tox
#'
#' This method continues at the current dose when the outcomes observed exactly
#' num_doses toxicity events in num_patients patients.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param num_tox Stick at current dose when this many toxicities are seen...
#' @param num_patients ...in this many patients
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' target <- 0.25
#' # Where a design might choose to deescalate:
#' model1 <- get_mtpi2(num_doses = 5, target = target, epsilon1 = 0.05,
#'                     epsilon2 = 0.05, exclusion_certainty = 0.95)
#' model1 %>% fit('1NNN 2NTN') %>% recommended_dose()
#' # you can alter that behaviour when a precise number of toxicities have been
#' # seen in a precise number of patients:
#' model2 <- get_mtpi2(num_doses = 5, target = target, epsilon1 = 0.05,
#'                     epsilon2 = 0.05, exclusion_certainty = 0.95) %>%
#'             stick_on_num_tox(num_tox = 1, num_patients = 3)
#' model2 %>% fit('1NNN 2NTN') %>% recommended_dose()
#' # The current dose is recommended instead.
stick_on_num_tox <- function(parent_selector_factory, num_tox, num_patients) {

  x <- list(
    parent = parent_selector_factory,
    num_tox = num_tox,
    num_patients = num_patients
  )
  class(x) <- c('stick_on_num_tox_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

stick_on_num_tox_selector <- function(parent_selector, num_tox, num_patients) {

  l <- list(
    parent = parent_selector,
    num_tox = num_tox,
    num_patients = num_patients
  )

  class(l) = c('stick_on_num_tox_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.stick_on_num_tox_selector_factory <- function(selector_factory,
                                                  outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(
    stick_on_num_tox_selector(
      parent_selector = parent_selector,
      num_tox = selector_factory$num_tox,
      num_patients = selector_factory$num_patients
    )
  )
}

# Selector interface

#' @export
continue.stick_on_num_tox_selector <- function(x, ...) {
  if(num_patients(x) > 0) {
    last_dose <- tail(doses_given(x), 1)
    n_d <- n_at_dose(x)[last_dose]
    tox_d <- tox_at_dose(x)[last_dose]
    if(n_d == x$num_patients & tox_d == x$num_tox) {
      return(TRUE)
    }
  }

  # By default:
  return(continue(x$parent))
}

#' @export
recommended_dose.stick_on_num_tox_selector <- function(x, ...) {
  if(num_patients(x) > 0) {
    last_dose <- tail(doses_given(x), 1)
    n_d <- n_at_dose(x)[last_dose]
    tox_d <- tox_at_dose(x)[last_dose]
    if(n_d == x$num_patients & tox_d == x$num_tox) {
      return(last_dose)
    }
  }

  # By default:
  return(recommended_dose(x$parent))
}

#' @export
print.stick_on_num_tox_selector <- function(x, ...) {
  .dose_selector_print(x, ...)
}

#' @export
as_tibble.stick_on_num_tox_selector <- function(x, ...) {
  .dose_selector_to_tibble(x, ...)
}

#' @export
summary.stick_on_num_tox_selector <- function(object, ...) {
  .dose_selector_summary(object, ...)
}
