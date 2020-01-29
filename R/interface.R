
# selector_factory interface
fit <- function(selector_factory, outcomes, ...) {
  UseMethod('fit')
}

# selector interface
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
