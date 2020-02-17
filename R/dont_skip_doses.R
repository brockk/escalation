
#' Prevent skipping of doses.
#'
#' This method optionally prevents dose selectors from skipping doses when
#' escalating and / or deescalating. The default is that skipping when
#' escalating is prevented but skipping when deescalating is permitted, but both
#' of these behaviours can be altered.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param when_escalating TRUE to prevent skipping when attempting to escalate.
#' @param when_deescalating TRUE to prevent skipping when attempting to
#' deescalate.
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
#'   dont_skip_doses()
#' fit1 <- model1 %>% fit('1NNN')
#'
#' model2 <- get_dfcrm(skeleton = skeleton, target = target)
#' fit2 <- model2 %>% fit('1NNN')
#'
#' # fit1 will not skip doses
#' fit1 %>% recommended_dose()
#' # But fit2 will:
#' fit2 %>% recommended_dose()
#'
#' # Similar demonstration for de-escalation
#' model1 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   dont_skip_doses(when_deescalating = TRUE)
#' fit1 <- model1 %>% fit('1NNN 2N 3TTT')
#'
#' model2 <- get_dfcrm(skeleton = skeleton, target = target)
#' fit2 <- model2 %>% fit('1NNN 2N 3TTT')
#'
#' # fit1 will not skip doses
#' fit1 %>% recommended_dose()
#' # But fit2 will:
#' fit2 %>% recommended_dose()
dont_skip_doses <- function(parent_selector_factory, when_escalating = TRUE,
                            when_deescalating = FALSE) {

  x <- list(
    parent = parent_selector_factory,
    when_escalating = when_escalating,
    when_deescalating = when_deescalating
  )
  class(x) <- c('dont_skip_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

dont_skip_selector <- function(parent_selector, when_escalating,
                               when_deescalating) {

  l <- list(
    parent = parent_selector,
    when_escalating = when_escalating,
    when_deescalating = when_deescalating
  )

  class(l) = c('dont_skip_selector', 'derived_dose_selector', 'selector')
  l
}

# Factory interface

#' @export
fit.dont_skip_selector_factory <- function(selector_factory, outcomes, ...) {
  parent_selector <- selector_factory$parent %>%
    fit(outcomes, ...)
  return(dont_skip_selector(
    parent_selector = parent_selector,
    when_escalating = selector_factory$when_escalating,
    when_deescalating = selector_factory$when_deescalating
  ))
}

# Selector interface

#' @export
recommended_dose.dont_skip_selector <- function(selector, ...) {
  parent_rec_d <- recommended_dose(selector$parent)
  if(num_patients(selector) == 0) {
    # No dose given, so just go with whatever parent proposes
    return(parent_rec_d)
  } else {
    last_d <- tail(doses_given(selector), 1)
    if(selector$when_escalating & parent_rec_d > last_d + 1) {
      return(as.integer(last_d + 1))
    } else if(selector$when_deescalating & parent_rec_d < last_d - 1) {
      return(as.integer(last_d - 1))
    } else {
      return(parent_rec_d)
    }
  }
}

#' @export
continue.dont_skip_selector <- function(selector, ...) {
  return(continue(selector$parent))
}
