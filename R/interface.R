
# selector_factory interface
fit <- function(selector_factory, outcomes, ...) {
  UseMethod('fit')
}

# selector interface
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


