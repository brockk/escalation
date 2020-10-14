
#' Select dose by BOIN's MTD-choosing algorithm.
#'
#' This method selects dose by the algorithm for identifying the maximum
#' tolerable dose (MTD) described in Yan et al. (2019). This class is intended
#' to be used when a BOIN trial has reached its maximum sample size. Thus, it
#' intends to make the final dose recommendation after the regular BOIN dose
#' selection algorithm, as implemented by \code{\link{get_boin}}, has gracefully
#' concluded a dose-finding trial. However, the class can be used in any
#' scenario where there is a target toxicity rate. See Examples.
#' Note - this class will not override the parent dose selector when the parent
#' is advocating no dose. Thus this class will not reinstate a dangerous dose.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param when Either of: 'finally' to select dose only when the parent
#' dose-selector has finished, by returning continue() == FALSE; or 'always'
#' to use this dose-selection algorithm for every dose decision. As per the
#' authors' original intentions, the default is 'finally'.
#' @param target We seek a dose with this probability of toxicity. If not
#' provided, the value will be sought from the parent dose-selector.
#' @param ... Extra args are passed to \code{\link[BOIN]{select.mtd}}.
#'
#' @return an object of type \code{\link{selector_factory}}.
#'
#' @export
#'
#' @examples
#' # This class is intended to make the final dose selection in a BOIN trial:
#' target <- 0.25
#' model <- get_boin(num_doses = 5, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   select_boin_mtd()
#'
#' outcomes <- '1NNN 2NTN 2NNN 3NTT'
#' model %>% fit(outcomes) %>% recommended_dose()
#'
#' # However, we can use this method to select dose at every dose decision:
#' model2 <- get_boin(num_doses = 5, target = target) %>%
#'   select_boin_mtd(when = 'always')
#' model2 %>% fit('1NNT') %>% recommended_dose()
#' model2 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' # and with any underlying model:
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   select_boin_mtd(when = 'always')
#' model3 %>% fit('1NNT') %>% recommended_dose()
#' model3 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' @references
#' Yan, F., Pan, H., Zhang, L., Liu, S., & Yuan, Y. (2019).
#' BOIN: An R Package for Designing Single-Agent and Drug-Combination
#' Dose-Finding Trials Using Bayesian Optimal Interval Designs.
#' Journal of Statistical Software, 27(November 2017), 0–35.
#' https://doi.org/10.18637/jss.v000.i00
select_boin_mtd <- function(parent_selector_factory,
                            when = c('finally', 'always'),
                            target = NULL,
                            ...) {

  when <- match.arg(when)

  x <- list(
    parent = parent_selector_factory,
    when = when,
    target = target,
    extra_args = list(...)
  )
  class(x) <- c('boin_mtd_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

#' @importFrom BOIN select.mtd
boin_mtd_dose_selector <- function(parent_selector,
                                   when = c('finally', 'always'),
                                   target = NULL,
                                   ...) {

  when <- match.arg(when)

  if(is.null(target)) {
    target <- tox_target(parent_selector)
    if(is.null(target)) {
      stop(paste0("Target toxicity probability is required when selecting dose",
                  " by BOIN's MTD algorithm. Could not fetch from parent."))
    }
  }

  boin_fit <- select.mtd(
    target = target,
    npts = n_at_dose(parent_selector),
    ntox = tox_at_dose(parent_selector),
    ...
  )

  l <- list(
    parent = parent_selector,
    when = when,
    target = target,
    boin_fit = boin_fit
  )

  class(l) = c('boin_mtd_dose_selector', 'derived_dose_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.boin_mtd_dose_selector_factory <- function(selector_factory, outcomes, ...) {

  parent_selector <- selector_factory$parent %>% fit(outcomes, ...)

  args <- list(
    parent_selector = parent_selector,
    when = selector_factory$when,
    target = selector_factory$target
  )
  do.call(boin_mtd_dose_selector, args = args)
}

# Selector interface

#' @export
recommended_dose.boin_mtd_dose_selector <- function(x, ...) {
  # Note: For yucks, BOIN::select.mtd returns 99 when sample size is 0, like NA
  # does not exist. Handle that.
  if(x$when == 'always') {
    if(num_patients(x) > 0)
      return(x$boin_fit$MTD)
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
      return(x$boin_fit$MTD)
    }
  }
}
