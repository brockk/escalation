
# selector_factory interface ----

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


#' Get function for simulating trials.
#'
#' This function does not need to be called by users. It is used internally.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#'
#' @return A function.
#' @export
simulation_function <- function(selector_factory) {
  UseMethod('simulation_function')
}

#' Get function for calculating dose pathways.
#'
#' This function does not need to be called by users. It is used internally.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#'
#' @return A function.
#' @export
dose_paths_function <- function(selector_factory) {
  UseMethod('dose_paths_function')
}



# selector interface ----

#' Target toxicity rate
#'
#' Get the target toxicity rate, if supported. NULL if not.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% tox_target()
tox_target <- function(x, ...) {
  UseMethod('tox_target')
}

#' Toxicity rate limit
#'
#' Get the maximum permissible toxicity rate, if supported. NULL if not.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' tox_limit(x)
tox_limit <- function(x, ...) {
  UseMethod('tox_limit')
}

#' Efficacy rate limit
#'
#' Get the minimum permissible efficacy rate, if supported. NULL if not.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' eff_limit(x)
eff_limit <- function(x, ...) {
  UseMethod('eff_limit')
}

#' Number of patients evaluated.
#'
#' Get the number of patients evaluated in a dose-finding trial.
#'
#' @param x Object of type \code{\link{selector}}.
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
num_patients <- function(x, ...) {
  UseMethod('num_patients')
}

#' Cohort numbers of evaluated patients.
#'
#' Get a vector of integers that reflect the cohorts to which the evaluated
#' patients belong.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return an integer vector
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% cohort()
cohort <- function(x, ...) {
  UseMethod('cohort')
}

#' Doses given to patients.
#'
#' Get a vector of the dose-levels that have been administered to patients.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return an integer vector
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% doses_given()
doses_given <- function(x, ...) {
  UseMethod('doses_given')
}

#' Binary toxicity outcomes.
#'
#' Get a vector of the binary toxicity outcomes for evaluated patients.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return an integer vector
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% tox()
tox <- function(x, ...) {
  UseMethod('tox')
}

#' Total number of toxicities seen.
#'
#' Get the number of toxicities seen in a dose-finding trial.
#'
#' @param x Object of type \code{\link{selector}}.
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
num_tox <- function(x, ...) {
  UseMethod('num_tox')
}

#' Binary efficacy outcomes.
#'
#' Get a vector of the binary efficacy outcomes for evaluated patients.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return an integer vector
#'
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' eff(x)
eff <- function(x, ...) {
  UseMethod('eff')
}

#' Total number of efficacies seen.
#'
#' Get the number of efficacies seen in a dose-finding trial.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return integer
#'
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' num_eff(x)
num_eff <- function(x, ...) {
  UseMethod('num_eff')
}

#' Model data-frame.
#'
#' Get the model data-frame for a dose-finding analysis, inlcuding columns for
#' patient id, cohort id, dose administered, and toxicity outcome. In some
#' scenarios, further columns are provided.
#'
#' @param x Object of type \code{\link{selector}}.
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
model_frame <- function(x, ...) {
  UseMethod('model_frame')
}

#' Number of doses.
#'
#' Get the number of doses under investigation in a dose-finding trial.
#'
#' @param x Object of type \code{\link{selector}}.
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
num_doses <- function(x, ...) {
  UseMethod('num_doses')
}

#' Dose indices
#'
#' Get the integers from 1 to the number of doses under investigation.
#'
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return an integer vector
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model <- get_dfcrm(skeleton = skeleton, target = target)
#' fit <- model %>% fit('1NNN 2NTN')
#' fit %>% dose_indices()
dose_indices <- function(x, ...) {
  UseMethod('dose_indices')
}

#' Recommended dose for next patient or cohort.
#'
#' Get the dose recommended for the next patient or cohort in a dose-finding
#' trial.
#'
#' @param x Object of type \code{\link{selector}}.
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
recommended_dose <- function(x, ...) {
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
#' @param x Object of type \code{\link{selector}}.
#' @param ... Extra args are passed onwards.
#'
#' @return logical
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
continue <- function(x, ...) {
  UseMethod('continue')
}

#' Number of patients treated at each dose.
#'
#' Get the number of patients evaluated at each dose under investigation.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return an integer vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% n_at_dose()
n_at_dose <- function(x, ...) {
  UseMethod('n_at_dose')
}

#' Number of patients treated at the recommended dose.
#'
#' Get the number of patients evaluated at the recommended dose.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return an integer
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% n_at_recommended_dose()
n_at_recommended_dose <- function(x, ...) {
  UseMethod('n_at_recommended_dose')
}

#' Percentage of patients treated at each dose.
#'
#' Get the percentage of patients evaluated at each dose under investigation.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% prob_administer()
prob_administer <- function(x, ...) {
  UseMethod('prob_administer')
}

#' Number of toxicities seen at each dose.
#'
#' Get the number of toxicities seen at each dose under investigation.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return an integer vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% tox_at_dose()
tox_at_dose <- function(x, ...) {
  UseMethod('tox_at_dose')
}

#' Observed toxicity rate at each dose.
#'
#' Get the empirical or observed toxicity rate seen at each dose under
#' investigation. This is simply the number of toxicities divded by the number
#' of patients evaluated.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% empiric_tox_rate()
empiric_tox_rate <- function(x, ...) {
  UseMethod('empiric_tox_rate')
}

#' Mean toxicity rate at each dose.
#'
#' Get the estimated mean toxicity rate at each dose under investigation. This
#' is a set of modelled statistics. The underlying models estimate toxicity
#' probabilities in different ways. If no model-based estimate of the mean is
#' available, this function will return a vector of NAs.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% mean_prob_tox()
mean_prob_tox <- function(x, ...) {
  UseMethod('mean_prob_tox')
}

#' Median toxicity rate at each dose.
#'
#' Get the estimated median toxicity rate at each dose under investigation. This
#' is a set of modelled statistics. The underlying models estimate toxicity
#' probabilities in different ways. If no model-based estimate of the median is
#' available, this function will return a vector of NAs.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% median_prob_tox()
median_prob_tox <- function(x, ...) {
  UseMethod('median_prob_tox')
}


#' Number of toxicities seen at each dose.
#'
#' Get the number of toxicities seen at each dose under investigation.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return an integer vector
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' eff_at_dose(x)
eff_at_dose <- function(x, ...) {
  UseMethod('eff_at_dose')
}

#' Observed efficacy rate at each dose.
#'
#' Get the empirical or observed efficacy rate seen at each dose under
#' investigation. This is simply the number of efficacies divded by the number
#' of patients evaluated.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' empiric_tox_rate(x)
empiric_eff_rate <- function(x, ...) {
  UseMethod('empiric_eff_rate')
}

#' Mean efficacy rate at each dose.
#'
#' Get the estimated mean efficacy rate at each dose under investigation. This
#' is a set of modelled statistics. The underlying models estimate efficacy
#' probabilities in different ways. If no model-based estimate of the mean is
#' available, this function will return a vector of NAs.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' mean_prob_eff(x)
mean_prob_eff <- function(x, ...) {
  UseMethod('mean_prob_eff')
}

#' Median efficacy rate at each dose.
#'
#' Get the estimated median efficacy rate at each dose under investigation. This
#' is a set of modelled statistics. The underlying models estimate efficacy
#' probabilities in different ways. If no model-based estimate of the median is
#' available, this function will return a vector of NAs.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' median_prob_eff(x)
median_prob_eff <- function(x, ...) {
  UseMethod('median_prob_eff')
}

#' Is each dose admissible?
#'
#' Get a vector of logical values reflecting whether each dose is admissible.
#' Admissibility is defined in different ways for different models, and may not
#' be defined at all in some models. For instance, in the TPI method, doses are
#' inadmissible when the posterior probability is high that the toxicity rate
#' exceeds the target value. In contrast, admissibility is not defined in the
#' general CRM model (but it can be added with auxiliary classes). In this
#' latter case, doses are implicitly considered to be admissible, by default.
#'
#' @param x Object of class \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' outcomes <- '1NNN 2TTT'
#'
#' # TPI example. This method defines admissibility.
#' fit1 <- get_tpi(num_doses = 5, target = 0.3, k1 = 1, k2 = 1.5,
#'                 exclusion_certainty = 0.95) %>%
#'   fit(outcomes)
#' fit1 %>% dose_admissible()
#'
#' # Ordinary CRM example with no admissibility function.
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' fit2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   fit(outcomes)
#' fit2 %>% dose_admissible()
#'
#' # Same CRM example with added admissibility function
#' fit3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_too_toxic(dose = 1, tox_threshold = target, confidence = 0.8) %>%
#'   fit(outcomes)
#' fit3 %>% dose_admissible()
dose_admissible <- function(x, ...) {
  UseMethod('dose_admissible')
}

#' Quantile of the toxicity rate at each dose.
#'
#' Get the estimated quantile of the toxicity rate at each dose under
#' investigation. This is a set of modelled statistics. The underlying models
#' estimate toxicity probabilities in different ways. If no model-based
#' estimate of the median is available, this function will return a vector of
#' NAs.
#'
#' @param x Object of class \code{\link{selector}}
#' @param p quantile probability, decimal value between 0 and 1
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% prob_tox_quantile(p = 0.9)
prob_tox_quantile <- function(x, p, ...) {
  UseMethod('prob_tox_quantile')
}

#' Probability that the toxicity rate exceeds some threshold.
#'
#' Get the probability that the toxicity rate at each dose exceeds some
#' threshold.
#'
#' @param x Object of type \code{\link{selector}}
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
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' # What is probability that tox rate at each dose exceeds target by >= 10%?
#' fit %>% prob_tox_exceeds(threshold = target + 0.1)
prob_tox_exceeds <- function(x, threshold, ...) {
  UseMethod('prob_tox_exceeds')
}

#' Quantile of the efficacy rate at each dose.
#'
#' Get the estimated quantile of the efficacy rate at each dose under
#' investigation. This is a set of modelled statistics. The underlying models
#' estimate efficacy probabilities in different ways. If no model-based
#' estimate of the median is available, this function will return a vector of
#' NAs.
#'
#' @param x Object of class \code{\link{selector}}
#' @param p quantile probability, decimal value between 0 and 1
#' @param ... arguments passed to other methods
#'
#' @return a numerical vector
#' @export
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' prob_tox_quantile(x, p = 0.9)
prob_eff_quantile <- function(x, p, ...) {
  UseMethod('prob_eff_quantile')
}

#' Probability that the efficacy rate exceeds some threshold.
#'
#' Get the probability that the efficacy rate at each dose exceeds some
#' threshold.
#'
#' @param x Object of type \code{\link{selector}}
#' @param threshold  Probability that efficacy rate exceeds what?
#' @param ... arguments passed to other methods
#'
#' @return numerical vector of probabilities
#'
#' @export
#'
#' @rdname prob_tox_exceeds
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' prob_tox_exceeds(x, threshold = 0.45)
prob_eff_exceeds <- function(x, threshold, ...) {
  UseMethod('prob_eff_exceeds')
}

#' Does this selector support sampling of outcomes?
#'
#' Learn whether this selector supports sampling of outcomes. For instance, is
#' it possible to get posterior samples of the probability of toxicity at each
#' dose? If true, prob_tox_samples will return a data-frame of samples.
#'
#' @param x Object of type \code{\link{selector}}
#' @param ... arguments passed to other methods
#'
#' @return logical
#'
#' @export
#'
#' @rdname supports_sampling
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% supports_sampling()
supports_sampling <- function(x, ...) {
  UseMethod('supports_sampling')
}

#' Get samples of the probability of toxicity.
#'
#' Get samples of the probability of toxicity. For instance, a Bayesian approach
#' that supports sampling would be expected to return posterior samples of the
#' probability of toxicity. If this class does not support sampling, this
#' function will raise an error. You can check whether this class supports
#' sampling by calling \code{\link{supports_sampling}}.
#'
#' @param x Object of type \code{\link{selector}}
#' @param tall logical, if FALSE, a wide data-frame is returned with columns
#' pertaining to the doses and column names the dose indices.
#' If TRUE, a tall data-frame is returned with data for all doses stacked
#' vertically. In this mode, column names will include \code{dose} and
#' \code{prob_tox}.
#' @param ... arguments passed to other methods
#'
#' @return data-frame like object
#'
#' @export
#'
#' @rdname prob_tox_samples
#'
#' @examples
#' # CRM example
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' outcomes <- '1NNN 2NTN'
#' fit <- get_dfcrm(skeleton = skeleton, target = target) %>% fit(outcomes)
#' fit %>% prob_tox_samples()
#' fit %>% prob_tox_samples(tall = TRUE)
prob_tox_samples <- function(x, tall = FALSE, ...) {
  UseMethod('prob_tox_samples')
}

#' Get samples of the probability of efficacy
#'
#' Get samples of the probability of efficacy For instance, a Bayesian approach
#' that supports sampling would be expected to return posterior samples of the
#' probability of toxicity. If this class does not support sampling, this
#' function will raise an error. You can check whether this class supports
#' sampling by calling \code{\link{supports_sampling}}.
#'
#' @param x Object of type \code{\link{selector}}
#' @param tall logical, if FALSE, a wide data-frame is returned with columns
#' pertaining to the doses and column names the dose indices.
#' If TRUE, a tall data-frame is returned with data for all doses stacked
#' vertically. In this mode, column names will include \code{dose} and
#' \code{prob_eff}.
#' @param ... arguments passed to other methods
#'
#' @return data-frame like object
#'
#' @export
#'
#' @rdname prob_tox_samples
#'
#' @examples
#' efftox_priors <- trialr::efftox_priors
#' p <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
#'                    beta_mean = 1.5482, beta_sd = 3.5018,
#'                    gamma_mean = 0.7367, gamma_sd = 2.5423,
#'                    zeta_mean = 3.4181, zeta_sd = 2.4406,
#'                    eta_mean = 0, eta_sd = 0.2,
#'                    psi_mean = 0, psi_sd = 1)
#' real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0)
#' model <- get_trialr_efftox(real_doses = real_doses,
#'                            efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
#'                            p_e = 0.1, p_t = 0.1,
#'                            eff0 = 0.5, tox1 = 0.65,
#'                            eff_star = 0.7, tox_star = 0.25,
#'                            priors = p, iter = 1000, chains = 1, seed = 2020)
#' x <- model %>% fit('1N 2E 3B')
#' prob_tox_samples(x, tall = TRUE)
prob_eff_samples <- function(x, tall = FALSE, ...) {
  UseMethod('prob_eff_samples')
}

# simulations interface ----

#' Probability of recommendation
#'
#' Get the probabilities that each of the doses under investigation is
#' recommended.
#'
#' @param x Object of type \code{\link{simulations}}.
#' @param ... arguments passed to other methods
#'
#' @return vector of probabilities
#' @export
#'
#' @examples
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox)
#' sims %>% prob_recommend
prob_recommend <- function(x, ...) {
  UseMethod('prob_recommend')
}

#' Duration of trials.
#'
#' Get the length of time that trials take to recruit all patients.
#'
#' @param x Object of type \code{\link{simulations}}.
#' @param ... arguments passed to other methods

#' @return vector of numerical times
#' @export
#'
#' @examples
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox)
#' sims %>% trial_duration
trial_duration <- function(x, ...) {
  UseMethod('trial_duration')
}
