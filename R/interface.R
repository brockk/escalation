
# selector_factory interface
fit <- function(selector_factory, outcomes, ...) {
  UseMethod('fit')
}

# selector interface
recommended_dose <- function(selector, ...) {
  UseMethod('recommended_dose')
}

n_at_dose <- function(selector, ...) {
  UseMethod('n_at_dose')
}
