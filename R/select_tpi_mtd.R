
#' Select dose by TPI's MTD-choosing algorithm.
#'
#' This method selects dose by the algorithm for identifying the maximum
#' tolerable dose (MTD) described in Ji et al. (2007). This class is intended
#' to be used when a TPI trial has reached its maximum sample size. Thus, it
#' intends to make the final dose recommendation after the regular TPI dose
#' selection algorithm, as implemented by \code{\link{get_tpi}}, including any
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
#' @param exclusion_certainty Numeric, threshold posterior certainty required to
#' exclude a dose for being excessively toxic. The authors discuss values in the
#' range 0.7 - 0.95. Set to a value > 1 to suppress the dose exclusion
#' mechanism. The authors use the Greek letter xi for this parameter.
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
#' # This class is intended to make the final dose selection in a mTPI2 trial:
#' target <- 0.25
#' model <- get_tpi(num_doses = 5, target = target,
#'                  k1 = 1, k2 = 1.5,
#'                  exclusion_certainty = 0.95) %>%
#'   stop_at_n(n = 12) %>%
#'   select_tpi_mtd(exclusion_certainty = 0.95)
#'
#' outcomes <- '1NNN 2NTN 2NNN 3NTT'
#' model %>% fit(outcomes) %>% recommended_dose()
#'
#' # However, since behaviour is modular in this package, we can use this method
#' # to select dose at every dose decision if we wanted:
#' model2 <- get_tpi(num_doses = 5, target = target,
#'                   k1 = 1, k2 = 1.5,
#'                   exclusion_certainty = 0.95) %>%
#'   select_tpi_mtd(when = 'always', exclusion_certainty = 0.95)
#' model2 %>% fit('1NNT') %>% recommended_dose()
#' model2 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' # and with any underlying model:
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   select_tpi_mtd(when = 'always', exclusion_certainty = 0.95)
#' model3 %>% fit('1NNT') %>% recommended_dose()
#' model3 %>% fit('1NNN 2NNT') %>% recommended_dose()
#'
#' @references
#' Ji, Y., Li, Y., & Bekele, B. N. (2007).
#' Dose-finding in phase I clinical trials based on toxicity probability
#' intervals.
#' Clinical Trials, 4(3), 235–244. https://doi.org/10.1177/1740774507079442
select_tpi_mtd <- function(parent_selector_factory,
                           when = c('finally', 'always'),
                           target = NULL,
                           exclusion_certainty,
                           alpha = 1, beta = 1,
                           ...) {

  when <- match.arg(when)

  x <- list(
    parent = parent_selector_factory,
    when = when,
    target = target,
    exclusion_certainty = exclusion_certainty,
    alpha = alpha,
    beta = beta,
    extra_args = list(...)
  )
  class(x) <- c('tpi_mtd_dose_selector_factory',
                'derived_dose_selector_factory',
                'selector_factory')
  return(x)
}

tpi_mtd_dose_selector <- function(parent_selector,
                                  when = c('finally', 'always'),
                                  target = NULL,
                                  exclusion_certainty,
                                  alpha, beta,
                                  ...) {

  when <- match.arg(when)

  if(is.null(target)) {
    target <- tox_target(parent_selector)
    if(is.null(target)) {
      stop(paste0("Target toxicity probability is required when selecting dose",
                  " by TPI's MTD algorithm. Could not fetch from parent."))
    }
  }

  l <- list(
    parent = parent_selector,
    when = when,
    target = target,
    exclusion_certainty = exclusion_certainty,
    alpha = alpha,
    beta = beta
  )

  class(l) = c('tpi_mtd_dose_selector',
               'derived_dose_selector',
               'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.tpi_mtd_dose_selector_factory <- function(selector_factory, outcomes,
                                              ...) {

  parent_selector <- selector_factory$parent %>% fit(outcomes, ...)

  args <- list(
    parent_selector = parent_selector,
    when = selector_factory$when,
    target = selector_factory$target,
    exclusion_certainty = selector_factory$exclusion_certainty,
    alpha = selector_factory$alpha,
    beta = selector_factory$beta
  )
  do.call(tpi_mtd_dose_selector, args = args)
}

# Selector interface

#' @export
mean_prob_tox.tpi_mtd_dose_selector <- function(x, ...) {
  # Use isotonic regression via the PAVA algorithm
  post_mean = (x$alpha + tox_at_dose(x)) / (x$alpha + x$beta + n_at_dose(x))
  post_var = (x$alpha + tox_at_dose(x)) *
    (x$beta + n_at_dose(x) - tox_at_dose(x)) /
    ((x$alpha + x$beta + n_at_dose(x))^2 * (x$alpha + x$beta + n_at_dose(x) + 1))
  post_mean = pava(post_mean, wt = 1 / post_var)
  return(post_mean)
}

#' @export
prob_tox_exceeds.tpi_mtd_dose_selector <- function(x, threshold, ...) {
  pava_bb_prob_tox_exceeds(x, threshold, alpha = x$alpha, beta = x$beta)
}

#' @export
dose_admissible.tpi_mtd_dose_selector <- function(x, ...) {
  n_d <- n_at_dose(x)
  t_d <- tox_at_dose(x)
  prob_unsafe <- prob_tox_exceeds(x, threshold = x$target)
  reject <- logical(length = num_doses(x))
  for(i in seq_along(reject)) {
    if(n_d[i] >= 2) {
      reject[i] <- prob_unsafe[i] >= x$exclusion_certainty
    } else {
      reject[i] <- FALSE # Implicitly
    }
  }
  # However, monotonic tox suggests doses higher than an inadmissible dose
  # are also inadmissible:
  cum_reject <- cumsum(reject) >= 1
  return(!cum_reject)
}

#' @export
#' @importFrom stats pbeta
recommended_dose.tpi_mtd_dose_selector <- function(x, ...) {

  # Note that this is legitimately different to mtpi_mtd! The algos for
  # resolving ties differ. However, it perfectly matches mtpi2_mtd. See the
  # papers!
  tpi_mtd <- function(x) {
    prob_tox <- mean_prob_tox(x)
    target <- tox_target(x)
    admissible <- dose_admissible(x)
    abs_delta <- abs(prob_tox - target)
    abs_delta[!admissible] <- NA

    if(sum(abs_delta == min(abs_delta, na.rm = TRUE), na.rm = TRUE) == 1) {
      # There is a single dose closest to target. Select that dose:
      return(which.min(abs_delta))
    } else {
      # We have several doses tied on distance from tox target.
      candidate <- abs_delta == min(abs_delta, na.rm = TRUE)
      # Calculate their mean tox
      pstar <- mean(prob_tox[candidate], na.rm = TRUE)
      if(pstar < target) {
        return(max(which(candidate)))
      } else {
        return(min(which(candidate)))
      }
    }
  }

  if(x$when == 'always') {
    if(num_patients(x) > 0)
      return(tpi_mtd(x))
    else
      return(recommended_dose(x$parent, ...))
  } else if(x$when == 'finally') {
    parent_d <- recommended_dose(x$parent, ...)
    parent_cont <- continue(x$parent)
    if(parent_cont) {
      # The parent is still going. Do not get in the way:
      return(parent_d)
    } else if(is.na(parent_d)){
      # Do not override an NA recommendation
      return(NA)
    } else {
      # The parent has stopped and recommends a non-NA dose. Get involved:
      return(tpi_mtd(x))
    }
  }
}
