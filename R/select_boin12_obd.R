
#' Select dose by BOIN12's OBD-choosing algorithm.
#'
#' This method selects dose by the algorithm for identifying the optimal
#' biological dose (OBD) described in Lin et al. (2020). This class is intended
#' to be used when a BOIN12 trial has reached its maximum sample size. Thus, it
#' intends to make the final dose recommendation after the regular BOIN12 dose
#' selection algorithm, as implemented by \code{\link{get_boin12}}, has
#' gracefully concluded a dose-finding trial. However, the class can be used in
#' any scenario where there is a limit toxicity rate. See Examples.
#' Note - this class will not override the parent dose selector when the parent
#' is advocating no dose. Thus this class will not reinstate a dangerous dose.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param when Either of: 'finally' to select dose only when the parent
#' dose-selector has finished, by returning continue() == FALSE; or 'always'
#' to use this dose-selection algorithm for every dose decision. As per the
#' authors' original intentions, the default is 'finally'.
#' @param tox_limit We seek a dose with toxicity probability no greater than.
#' If not provided, the value will be sought from the parent dose-selector.
#' @param ... Extra args are ignored.
#'
#' @return an object of type \code{\link{selector_factory}}.
#'
#' @export
#'
#' @examples
#' # This class is intended to make the final dose selection in a BOIN12 trial:
#' tox_limit <- 0.35
#' model <- get_boin12(num_doses = 5, tox_limit = tox_limit) %>%
#'   stop_at_n(n = 12) %>%
#'   select_boin12_obd()
#'
#' outcomes <- '1NNN 2NTN 2NNN 3NTT'
#' model %>% fit(outcomes) %>% recommended_dose()
#'
#' # However, since behaviour is modular in this package, we can use this method
#' # to select dose at every dose decision:
#' model2 <- get_boin12(num_doses = 5, tox_limit = tox_limit) %>%
#'   select_boin12_obd(when = 'always')
#' model2 %>% fit('1NNT') %>% recommended_dose()
#' model2 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' # and with any underlying model:
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' model3 <- get_dfcrm(skeleton = skeleton, target = tox_limit) %>%
#'   select_boin12_obd(when = 'always', tox_limit = tox_limit)
#' model3 %>% fit('1NNT') %>% recommended_dose()
#' model3 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' @references
#' Lin, R., Zhou, Y., Yan, F., Li, D., & Yuan, Y. (2020).
#' BOIN12: Bayesian optimal interval phase I/II trial design for utility-based
#' dose finding in immunotherapy and targeted therapies.
#' JCO precision oncology, 4, 1393-1402.
select_boin12_obd <- function(parent_selector_factory,
                              when = c('finally', 'always'),
                              tox_limit = NULL,
                              ...) {

  when <- match.arg(when)

  x <- list(
    parent = parent_selector_factory,
    when = when,
    tox_limit = tox_limit,
    extra_args = list(...)
  )
  class(x) <- c('boin12_obd_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

boin12_obd_dose_selector <- function(parent_selector,
                                     when = c('finally', 'always'),
                                     tox_limit = NULL,
                                     ...) {

  when <- match.arg(when)

  if(is.null(tox_limit)) {
    tox_limit <- tox_limit(parent_selector)
    if(is.null(tox_limit)) {
      stop(paste0("Toxicity limit probability is required when selecting dose",
                  " by BOIN12's OBD algorithm. Could not fetch from parent."))
    }
  }

  l <- list(
    parent = parent_selector,
    when = when,
    tox_limit = tox_limit
  )

  class(l) = c('boin12_obd_dose_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.boin12_obd_dose_selector_factory <- function(selector_factory, outcomes,
                                                 ...) {

  parent_selector <- selector_factory$parent %>% fit(outcomes, ...)

  args <- list(
    parent_selector = parent_selector,
    when = selector_factory$when,
    tox_limit = selector_factory$tox_limit
  )
  do.call(boin12_obd_dose_selector, args = args)
}

# Selector interface

#' @export
recommended_dose.boin12_obd_dose_selector <- function(x, ...) {

  boin12obd <- function(x) {
    # Start with empiric tox rate
    etr <- empiric_tox_rate(x)
    names(etr) <- dose_indices(x)
    # Apply isotonic regression to just the given doses
    given <- n_at_dose(x) > 0
    etr_pava <- pava(etr[given])
    # Identify MTD
    # mtd_u <- abs(etr_pava - x$tox_limit)
    mtd_u <- abs(etr_pava - tox_limit(x))
    mtd_tox <- min(mtd_u)
    mtd_loc <- tail(mtd_u[mtd_u == mtd_tox], 1)
    mtd <- as.integer(names(mtd_loc))
    # Select maximal utility dose at or below MTD
    di <- dose_indices(x)
    obd <- which.max(utility(x)[di[di <= mtd]])
    obd
  }

  if(x$when == 'always') {
    if(num_patients(x) > 0)
      return(boin12obd(x))
    else
      return(recommended_dose(x$parent))
  } else if(x$when == 'finally') {
    parent_d <- recommended_dose(x$parent)
    parent_cont <- continue(x$parent)
    if(parent_cont) {
      # The parent is still going. Do not get in the way:
      return(parent_d)
    } else if(is.na(parent_d)){
      # Do not override an NA recommendation
      return(NA)
    } else {
      # The parent has stopped and recommends a non-NA dose. Get involved:
      return(boin12obd(x))
    }
  }
}
