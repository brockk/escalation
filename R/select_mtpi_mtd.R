
#' Select dose by mTPI's MTD-choosing algorithm.
#'
#' This method selects dose by the algorithm for identifying the maximum
#' tolerable dose (MTD) described in Ji et al. (2010). This class is intended
#' to be used when a mTPI trial has reached its maximum sample size. Thus, it
#' intends to make the final dose recommendation after the regular mTPI dose
#' selection algorithm, as implemented by \code{\link{get_mtpi}}, including any
#' additional behaviours that govern stopping (etc), has gracefully concluded a
#' dose-finding trial. However, the class can be used in any scenario where
#' there is a target toxicity rate. See Examples. Note - this class will not
#' override the parent dose selector when the parent is advocating no dose. Thus
#' this class will not reinstate a dangerous dose.
#'
#' @param parent_selector_factory Object of type \code{\link{selector_factory}}.
#' @param when Either of: 'finally' to select dose only when the parent
#' dose-selector has finished, by returning continue() == FALSE; or 'always'
#' to use this dose-selection algorithm for every dose decision. As per the
#' authors' original intentions, the default is 'finally'.
#' @param target We seek a dose with this probability of toxicity. If not
#' provided, the value will be sought from the parent dose-selector.
#' @param alpha First shape parameter of the beta prior distribution on the
#' probability of toxicity.
#' @param beta Second shape parameter of the beta prior distribution on the
#' probability of toxicity.
#' @param ... Extra args are passed onwards.
#'
#' @return an object of type \code{\link{selector_factory}}.
#'
#' @export
#'
#' @examples
#' # This class is intended to make the final dose selection in a mTPI trial:
#' target <- 0.25
#' model <- get_mtpi(num_doses = 5, target = target,
#'                   epsilon1 = 0.05, epsilon2 = 0.05,
#'                   exclusion_certainty = 0.95) %>%
#'   stop_at_n(n = 12) %>%
#'   select_mtpi_mtd()
#'
#' outcomes <- '1NNN 2NTN 2NNN 3NTT'
#' model %>% fit(outcomes) %>% recommended_dose()
#'
#' # However, since behaviour is modular in this package, we can use this method
#' # to select dose at every dose decision if we wanted:
#' model2 <- get_mtpi(num_doses = 5, target = target,
#'                    epsilon1 = 0.05, epsilon2 = 0.05,
#'                    exclusion_certainty = 0.95) %>%
#'   select_mtpi_mtd(when = 'always')
#' model2 %>% fit('1NNT') %>% recommended_dose()
#' model2 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' # and with any underlying model:
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   select_mtpi_mtd(when = 'always')
#' model3 %>% fit('1NNT') %>% recommended_dose()
#' model3 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' @references
#' Ji, Y., Liu, P., Li, Y., & Bekele, B. N. (2010).
#'  A modified toxicity probability interval method for dose-finding trials.
#'  Clinical Trials, 7(6), 653-663. https://doi.org/10.1177/1740774510382799
#'
#' Ji, Y., & Yang, S. (2017).
#' On the Interval-Based Dose-Finding Designs, 1-26.
#' Retrieved from https://arxiv.org/abs/1706.03277
select_mtpi_mtd <- function(parent_selector_factory,
                            when = c('finally', 'always'),
                            target = NULL,
                            alpha = 1, beta = 1,
                            ...) {

  when <- match.arg(when)

  x <- list(
    parent = parent_selector_factory,
    when = when,
    target = target,
    alpha = alpha,
    beta = beta,
    extra_args = list(...)
  )
  class(x) <- c('mtpi_mtd_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

mtpi_mtd_dose_selector <- function(parent_selector,
                                   when = c('finally', 'always'),
                                   target = NULL,
                                   alpha, beta,
                                   ...) {

  when <- match.arg(when)

  if(is.null(target)) {
    target <- tox_target(parent_selector)
    if(is.null(target)) {
      stop(paste0("Target toxicity probability is required when selecting dose",
                  " by mTPI's MTD algorithm. Could not fetch from parent."))
    }
  }

  l <- list(
    parent = parent_selector,
    when = when,
    target = target,
    alpha = alpha,
    beta = beta
  )

  class(l) = c('mtpi_mtd_dose_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.mtpi_mtd_dose_selector_factory <- function(selector_factory, outcomes,
                                               ...) {

  parent_selector <- selector_factory$parent %>% fit(outcomes, ...)

  args <- list(
    parent_selector = parent_selector,
    when = selector_factory$when,
    target = selector_factory$target,
    alpha = selector_factory$alpha,
    beta = selector_factory$beta
  )
  do.call(mtpi_mtd_dose_selector, args = args)
}

# Selector interface

#' @export
mean_prob_tox.mtpi_mtd_dose_selector <- function(x, ...) {
  # Use isotonic regression via the PAVA algorithm
  post_mean = (x$alpha + tox_at_dose(x)) / (x$alpha + x$beta + n_at_dose(x))
  post_var = (x$alpha + tox_at_dose(x)) *
    (x$beta + n_at_dose(x) - tox_at_dose(x)) /
    ((x$alpha + x$beta + n_at_dose(x))^2 * (x$alpha + x$beta + n_at_dose(x) + 1))
  post_mean = pava(post_mean, wt = 1 / post_var)
  return(post_mean)
}

#' @export
recommended_dose.mtpi_mtd_dose_selector <- function(x, ...) {

  prob_tox <- mean_prob_tox(x)
  target <- tox_target(x)

  abs_delta <- abs(prob_tox - target)
  if(sum(abs_delta == min(abs_delta)) == 1) {
    # There is a single dose closest to target. Select that dose:
    return(which.min(abs_delta))
  } else {
    # We have several doses tied on distance from tox target.
    delta <- prob_tox - target
    if(sum(delta == -min(abs_delta)) == 1) {
      # There is a unique dose at (target - q). Select that dose:
      return(min(which(delta == -min(abs_delta))))
    } else {
      # There are several doses at (prob_tox - tox_target). Select the highest:
      return(max(which(delta == -min(abs_delta))))
    }
  }
}
