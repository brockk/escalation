
#' Make untested and unrecommended doses inadmissible.
#'
#' This method sets untested and unrecommended doses to inadmissible,
#' irrespective the view of the parent selector.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   dont_skip_doses() %>%
#'   unadmit_untested()
#' fit1 <- model1 %>% fit('1NNN')
#'
#' # Dose 1 has been tested. Dose 2 is recommended. All other doses are not
#' # admissible:
#' fit1 %>% dose_admissible()
unadmit_untested <- function(parent_selector_factory) {

  x <- list(
    parent = parent_selector_factory
  )
  class(x) <- c('unadmit_untested_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

unadmit_untested_selector <- function(parent_selector) {

  l <- list(
    parent = parent_selector
  )

  class(l) = c('unadmit_untested_selector', 'derived_dose_selector', 'selector')
  l
}

# Factory interface

#' @export
fit.unadmit_untested_selector_factory <- function(selector_factory, outcomes,
                                                  ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(unadmit_untested_selector(
    parent_selector = parent_selector
  ))
}

# Selector interface

#' #' @export
#' recommended_dose.dont_skip_selector <- function(x, ...) {
#'   parent_rec_d <- recommended_dose(x$parent)
#'   if(length(parent_rec_d) > 1) {
#'     stop("dont_skip_selector does not work with dose combinations")
#'   }
#'   if(num_patients(x) == 0 | is.na(parent_rec_d)) {
#'     # No dose given, or parent selectc no dose, then just go with that
#'     return(parent_rec_d)
#'   } else {
#'     last_d <- tail(doses_given(x), 1)
#'     if(x$when_escalating & parent_rec_d > last_d + 1) {
#'       return(as.integer(last_d + 1))
#'     } else if(x$when_deescalating & parent_rec_d < last_d - 1) {
#'       return(as.integer(last_d - 1))
#'     } else {
#'       return(parent_rec_d)
#'     }
#'   }
#' }

#' @export
dose_admissible.unadmit_untested_selector <- function(x, ...) {
  parent_rec_d <- recommended_dose(x$parent)
  parent_admiss <- dose_admissible(x$parent)
  n_d <- n_at_dose(x)
  admiss <- parent_admiss
  admiss[n_d == 0] <- FALSE
  if(!is.na(parent_rec_d)) {
    admiss[parent_rec_d] <- TRUE
  }
  return(admiss)
}

#' @export
print.unadmit_untested_selector <- function(x, ...) {
  .dose_selector_print(x, ...)
}

#' @export
as_tibble.unadmit_untested_selector <- function(x, ...) {
  .dose_selector_to_tibble(x, ...)
}

#' @export
summary.unadmit_untested_selector <- function(object, ...) {
  .dose_selector_summary(object, ...)
}
