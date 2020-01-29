
# selector_factory interface
fit <- function(selector_factory, outcomes, ...) {
  UseMethod('fit')
}

# selector interface
num_patients <- function(selector, ...) {
  UseMethod('num_patients')
}

cohort <- function(selector, ...) {
  UseMethod('cohort')
}

doses_given <- function(selector, ...) {
  UseMethod('doses_given')
}

tox <- function(selector, ...) {
  UseMethod('tox')
}

model_frame <- function(selector, ...) {
  UseMethod('model_frame')
}

num_doses <- function(selector, ...) {
  UseMethod('num_doses')
}

recommended_dose <- function(selector, ...) {
  UseMethod('recommended_dose')
}

continue <- function(selector, ...) {
  UseMethod('continue')
}

n_at_dose <- function(selector, ...) {
  UseMethod('n_at_dose')
}

tox_at_dose <- function(selector, ...) {
  UseMethod('tox_at_dose')
}

empiric_tox_rate <- function(selector, ...) {
  UseMethod('empiric_tox_rate')
}

mean_prob_tox <- function(selector, ...) {
  UseMethod('mean_prob_tox')
}

median_prob_tox <- function(selector, ...) {
  UseMethod('median_prob_tox')
}

#' Probability that the rate of toxicity exceeds some threshold.
#'
#' @param selector dose selector, object of class \code{\link{selector}
#' @param threshold  Probability that toxicity rate exceeds what?
#' @param ... arguments passed to other methods
#' @return numerical vector of probabilities
#' @export
#' @rdname prob_tox_exceeds
#' @examples
#' 1==1
prob_tox_exceeds <- function(selector, threshold, ...) {
  UseMethod('prob_tox_exceeds')
}


