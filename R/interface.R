
# selector_factory interface

#' Fit a dose-finding model.
#'
#' Fit a dose-finding model to some outcomes.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#' @param outcomes Outcome string. See \code{\link{parse_phase1_outcomes}}.
#' @param ... Extra args are passed onwards.
#'
#' @return Object of generic type \code{\link{selector}}.
#'
#' @export
#'
#' @seealso \code{\link{selector}}, \code{\link{selector_factory}}
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% recommended_dose()  # Etc
fit <- function(selector_factory, outcomes, ...) {
  UseMethod('fit')
}

# selector interface

#' Number of patients evaluated.
#'
#' Get the number of patients evaluated in a dose-finding trial.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% num_patients()
num_patients <- function(selector, ...) {
  UseMethod('num_patients')
}

#' Cohort numbers of evaluated patients.
#'
#' Get a vector of integers that reflect the cohorts to which the evaluated
#' patients belong.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% cohort()
cohort <- function(selector, ...) {
  UseMethod('cohort')
}

#' Doses given to patients.
#'
#' Get a vector of the dose-levels that have been administered to patients.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% doses_given()
doses_given <- function(selector, ...) {
  UseMethod('doses_given')
}

#' Binary toxicity outcomes.
#'
#' Get a vector of the binary toxicity outcomes for evaluated patients.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% tox()
tox <- function(selector, ...) {
  UseMethod('tox')
}

#' Total number of toxicities seen.
#'
#' Get the number of toxicities seen in a dose-finding trial.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% num_tox()
num_tox <- function(selector, ...) {
  UseMethod('num_tox')
}

#' Model data-frame.
#'
#' Get the model data-frame for a dose-finding analysis, inlcuding columns for
#' patient id, cohort id, dose administered, and toxicity outcome. In some
#' scenarios, further columns are provided.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return \code{\link[tibble]{tibble}}, which acts like a \code{data.frame}.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% model_frame()
model_frame <- function(selector, ...) {
  UseMethod('model_frame')
}

#' Number of doses.
#'
#' Get the number of doses under investigation in a dose-finding trial.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% num_doses()
num_doses <- function(selector, ...) {
  UseMethod('num_doses')
}

#' Dose indices
#'
#' Get the integers from 1 to the number of doses under investigation.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% dose_indices()
dose_indices <- function(selector, ...) {
  UseMethod('dose_indices')
}

#' Recommended dose for next patient or cohort.
#'
#' Get the dose recommended for the next patient or cohort in a dose-finding
#' trial.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% recommended_dose()
recommended_dose <- function(selector, ...) {
  UseMethod('recommended_dose')
}

#' Should this dose-finding experiment continue?
#'
#' Should this dose-finding experiment continue? Or have circumstances prevailed
#' that dictate this trial should stop? This method is critical to the automatic
#' calculation of statistical operating characteristics and dose-pathways. You
#' add stopping behaviours to designs using calls like \code{\link{stop_at_n}}
#' and \code{\link{stop_when_too_toxic}}.
#'
#' @param selector Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#' fit1 <- model1 %>% fit('1NNN 2NTN')
#' fit1 %>% continue()
#'
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 6)
#' fit2 <- model2 %>% fit('1NNN 2NTN')
#' fit2 %>% continue()
continue <- function(selector, ...) {
  UseMethod('continue')
}

#' Number of patients treated at each dose.
#'
#' Get the number of patients evaluated at each dose under investigation.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return integer
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% n_at_dose()
n_at_dose <- function(selector, ...) {
  UseMethod('n_at_dose')
}

#' Percentage of patients treated at each dose.
#'
#' Get the percentage of patients evaluated at each dose under investigation.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return integer
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% prob_administer()
prob_administer <- function(selector, ...) {
  UseMethod('prob_administer')
}

#' Number of toxicities seen at each dose.
#'
#' Get the number of toxicities seen at each dose under investigation.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return integer
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% tox_at_dose()
tox_at_dose <- function(selector, ...) {
  UseMethod('tox_at_dose')
}

#' Observed toxicity rate at each dose.
#'
#' Get the empirical or observed toxicity rate seen at each dose under
#' investigation. This is simply the number of toxicities divded by the number
#' of patients evaluated.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return numeric
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% empiric_tox_rate()
empiric_tox_rate <- function(selector, ...) {
  UseMethod('empiric_tox_rate')
}

#' Mean toxicity rate at each dose.
#'
#' Get the estimated mean toxicity rate at each dose under investigation. This
#' is a set of modelled statistics. The underlying models estimate toxicity
#' probabilities in different ways. If no model-based estimate of the mean is
#' available, this function will return a vector of NAs.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return numeric
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% mean_prob_tox()
mean_prob_tox <- function(selector, ...) {
  UseMethod('mean_prob_tox')
}

#' Median toxicity rate at each dose.
#'
#' Get the estimated median toxicity rate at each dose under investigation. This
#' is a set of modelled statistics. The underlying models estimate toxicity
#' probabilities in different ways. If no model-based estimate of the median is
#' available, this function will return a vector of NAs.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return numeric
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% median_prob_tox()
median_prob_tox <- function(selector, ...) {
  UseMethod('median_prob_tox')
}

#' Quantile of the toxicity rate at each dose.
#'
#' Get the estimated quantile of the toxicity rate at each dose under
#' investigation. This is a set of modelled statistics. The underlying models
#' estimate toxicity probabilities in different ways. If no model-based
#' estimate of the median is available, this function will return a vector of
#' NAs.
#'
#' @param selector Object of class \code{\link{selector}}
#' @param p quantile probability, decimal value between 0 and 1
#' @param ... arguments passed to other methods
#'
#' @return numeric
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% prob_tox_quantile(p = 0.9)
prob_tox_quantile <- function(selector, p, ...) {
  UseMethod('prob_tox_quantile')
}

#' Probability that the toxicity rate exceeds some threshold.
#'
#' Get the probability that the toxicity rate at each dose exceeds some
#' threshold.
#'
#' @param selector Object of type \code{\link{selector}}
#' @param threshold  Probability that toxicity rate exceeds what?
#' @param ... arguments passed to other methods
#'
#' @return numerical vector of probabilities
#'
#' @export
#'
#' @rdname prob_tox_exceeds
#'
#' @examples
#' 1==1
prob_tox_exceeds <- function(selector, threshold, ...) {
  UseMethod('prob_tox_exceeds')
}
