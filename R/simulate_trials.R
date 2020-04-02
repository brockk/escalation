
#' Simulate clinical trials.
#'
#' @description
#' This function takes a \code{\link{selector_factory}}, such as that returned
#' by \code{\link{get_dfcrm}}, \code{\link{get_boin}} or
#' \code{\link{get_three_plus_three}}, and conducts many notional clinical
#' trials. We conduct simulations to learn about the operating characteristics
#' of adaptive trial designs.
#'
#' @details
#' By default, dose decisions in simulated trials are made after each cohort of
#' 3 patients. This can be changed by providing a function that simulates the
#' arrival of new patients. The new patients will be added to the existing
#' patients and the model will be fit to the set of all patients.
#' The function that simulates patient arrivals should take as a single
#' parameter a data-frame with one row for each existing patient and columns
#' including cohort, patient, dose, tox, time (and possibly also eff and weight,
#' if a phase I/II or time-to-event method is used). The provision of data on
#' the existing patients allows the patient sampling function to be adaptive.
#' The function should return a data-frame with a row for each new patient and a
#' column for time_delta, the time between the arrival of this patient and the
#' previous, as in \code{\link{cohorts_of_n}}. See Examples.
#'
#' This method can simulate the culmination of trials that are partly completed.
#' We just have to specify the outcomes already observed via the
#' \code{previous_outcomes} parameter. Each simulated trial will commence from
#' those outcomes seen thus far. See Examples.
#'
#' We can specify the immediate next dose by specifying \code{next_dose}.
#' If omitted, the next dose is calculated by invoking the model on the outcomes
#' seen thus far.
#'
#' Designs must eventually choose to stop the trial. However, some selectors
#' like those derived from \code{\link{get_dfcrm}} offer no default stopping
#' method. You may need to append stopping behaviour to your selector via
#' something like \code{\link{stop_at_n}} or \code{\link{stop_when_n_at_dose}},
#' etc. To safeguard against simulating runaway trials that never end, the
#' function will halt a simulated trial after 30 invocations of the dose-selection
#' decision. To breach this limit, specify \code{i_like_big_trials = TRUE} in
#' the function call. However, when you forego the safety net, the onus is on
#' you to write selectors that will eventually stop the trial! See Examples.
#'
#' The model is fit to the prevailing data at each dose selection point.
#' By default, only the final model fit for each simulated trial is retained.
#' This is done to conserve memory. With a high number of simulated trials,
#' storing many model fits per trial may cause the executing machine to run out
#' of memory. However, you can force this method to retain all model fits by
#' specifying \code{return_all_fits = TRUE}. See Examples.
#'
#' @param selector_factory Object of type \code{\link{selector_factory}}.
#' @param num_sims integer, number of trial iterations to simulate.
#' @param true_prob_tox numeric vector of true but unknown toxicity
#' probabilities
#' @param ... Extra args are passed onwards.
#'
#' @seealso \code{\link{simulations}}
#' @seealso \code{\link{selector_factory}}
#' @seealso \code{\link{get_dfcrm}}
#' @seealso \code{\link{get_boin}}
#' @seealso \code{\link{get_three_plus_three}}
#' @seealso \code{\link{cohorts_of_n}}
#'
#' @return Object of type \code{\link{simulations}}.
#' @export
#'
#' @examples
#' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#'
#' # Regular usage examples, for 3+3:
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)
#' # and continual reassessment method:
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)
#'
#' # Lots of useful information is contained in the returned object:
#' sims %>% num_patients()
#' sims %>% num_doses()
#' sims %>% dose_indices()
#' sims %>% n_at_dose()
#' sims %>% n_at_recommended_dose()
#' sims %>% tox_at_dose()
#' sims %>% num_tox()
#' sims %>% recommended_dose()
#' sims %>% prob_administer()
#' sims %>% prob_recommend()
#' sims %>% trial_duration()
#'
#' # By default, dose decisions are made after each cohort of 3 patients. See
#' # Details. To override, specify an alternative function via the
#' # sample_patient_arrivals parameter. E.g. to use cohorts of 2, we run:
#' patient_arrivals_func <- function(current_data) cohorts_of_n(n = 2)
#' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#'     sample_patient_arrivals = patient_arrivals_func)
#'
#' # To simulate the culmination of trials that are partly completed, specify
#' # the outcomes already observed via the previous_outcomes parameter:
#' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' stop_at_n(n = 12) %>%
#' simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#'                 previous_outcomes = '1NTN')
#' # Outcomes can be described by above outcome string method or data-frame:
#'   previous_outcomes <- data.frame(
#'     patient = 1:3,
#'     cohort = c(1, 1, 1),
#'     tox = c(0, 1, 0),
#'     dose = c(1, 1, 1)
#'   )
#' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#'                   previous_outcomes = previous_outcomes)
#'
#' # We can specify the immediate next dose:
#' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 12) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#'                   next_dose = 5)
#'
#' # By default, the method will stop simulated trials after 30 dose selections.
#' # To suppress this, specify i_like_big_trials = TRUE. However, please take
#' # care to specify selectors that will eventually stop!
#' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_at_n(n = 99) %>%
#'   simulate_trials(num_sims = 1, true_prob_tox = true_prob_tox,
#'                   i_like_big_trials = TRUE)
#'
#' # By default, only the final model fit is retained for each simulated trial.
#' # To retain all interim model fits, specify return_all_fits = TRUE.
#' sims <- get_three_plus_three(num_doses = 5) %>%
#'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#'                   return_all_fits = TRUE)
#' # Verify that there are now many analyses per trial with:
#' sapply(sims$fits, length)
simulate_trials <- function(selector_factory, num_sims, true_prob_tox, ...) {
  sim_func <- simulation_function(selector_factory)
  l <- lapply(
    1:num_sims,
    function(x) sim_func(selector_factory, true_prob_tox, ...)
  )
  simulations(fits = l, true_prob_tox = true_prob_tox)
}
