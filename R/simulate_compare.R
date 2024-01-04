
#' Simulate clinical trials for several designs using common patients.
#'
#' @description This function takes a list of several
#'   \code{\link{selector_factory}}s, such as those returned by
#'   \code{\link{get_dfcrm}}, \code{\link{get_boin}} or
#'   \code{\link{get_three_plus_three}}, and conducts many notional clinical
#'   trials. The simulated patients in the trials are common across designs. For
#'   example, in a comparison of the three designs mentioned above, the first
#'   simulated CRM trial uses the same notional patients as the first simulated
#'   BOIN trial, etc. Using common patients within iterate across designs
#'   reduces MCMC errors of comparisons, so this method is efficient for
#'   comparing designs. See Sweeting et al. for full details.
#'
#' @details By default, dose decisions in simulated trials are made after each
#'   cohort of 3 patients. This can be changed by providing a function by the
#'   \code{sample_patient_arrivals} parameter that simulates the arrival of new
#'   patients. The new patients will be added to the existing patients and the
#'   model will be fit to the set of all patients. The function that simulates
#'   patient arrivals should take as a single parameter a data-frame with one
#'   row for each existing patient and columns including cohort, patient, dose,
#'   tox, time (and possibly also eff and weight, if a phase I/II or
#'   time-to-event method is used). The provision of data on the existing
#'   patients allows the patient sampling function to be adaptive. The function
#'   should return a data-frame with a row for each new patient and a column for
#'   time_delta, the time between the arrival of this patient and the previous,
#'   as in \code{\link{cohorts_of_n}}. See Examples.
#'
#'   This method can simulate the culmination of trials that are partly
#'   completed. We just have to specify the outcomes already observed via the
#'   \code{previous_outcomes} parameter. Each simulated trial will commence from
#'   those outcomes seen thus far. See Examples.
#'
#'   We can specify the immediate next dose by specifying \code{next_dose}. If
#'   omitted, the next dose is calculated by invoking the model on the outcomes
#'   seen thus far.
#'
#'   Designs must eventually choose to stop the trial. Some designs, like 3+3,
#'   have intrinsic stopping rules. However, some selectors like those derived
#'   from \code{\link{get_dfcrm}} offer no default stopping method. You may need
#'   to append stopping behaviour to your selector via something like
#'   \code{\link{stop_at_n}} or \code{\link{stop_when_n_at_dose}}, etc. To
#'   safeguard against simulating runaway trials that never end, the function
#'   will halt a simulated trial after 30 invocations of the dose-selection
#'   decision. To breach this limit, specify \code{i_like_big_trials = TRUE} in
#'   the function call. However, when you forego the safety net, the onus is on
#'   you to write selectors that will eventually stop the trial! See Examples.
#'
#'   The model is fit to the prevailing data at each dose selection point. By
#'   default, only the final model fit for each simulated trial is retained.
#'   This is done to conserve memory. With a high number of simulated trials,
#'   storing many model fits per trial may cause the executing machine to run
#'   out of memory. However, you can force this method to retain all model fits
#'   by specifying \code{return_all_fits = TRUE}. See Examples.
#'
#' @param designs list, mapping design names to objects of type
#' \code{\link{selector_factory}}.
#' @param num_sims integer, number of trial iterations to simulate.
#' @param true_prob_tox numeric vector of true but unknown toxicity
#'   probabilities
#' @param true_prob_eff numeric vector of true but unknown efficacy
#'   probabilities. NULL if efficacy not analysed.
#' @param patient_samples Optional list of length \code{num_sims}, where each
#' element is an instance of \code{\link{PatientSample}} or a subclass like
#' \code{\link{CorrelatedPatientSample}}. These objects control the occurrence
#' of toxicity and efficacy events in patients. They are specifiable to allow
#' fine-grained control to users. See the vignette on Simulation.
#' @param rho Optional correlation between -1 and 1 for the latent uniform
#' variables that determine toxicity and efficacy events. Non-correlated events
#' is the default.
#' @param return_patient_samples TRUE to get the list of patient sample objects
#' returned in the patient_samples attribute of the retured object.
#' @param ... Extra args are passed onwards.
#'
#' @seealso \code{\link{simulations}}
#' @seealso \code{\link{selector_factory}}
#' @seealso \code{\link{get_dfcrm}}
#' @seealso \code{\link{get_boin}}
#' @seealso \code{\link{get_three_plus_three}}
#' @seealso \code{\link{cohorts_of_n}}
#'
#' @return object of type \code{\link{simulations_collection}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Don't run on build because they exceed CRAN time limit
#'
#' # In a five-dose scenario, we have assumed probabilities for Prob(tox):
#' true_prob_tox <- c(0.05, 0.10, 0.15, 0.18, 0.45)
#' # and Prov(eff):
#' true_prob_eff <- c(0.40, 0.50, 0.52, 0.53, 0.53)
#'
#' # Let us compare two BOIN12 variants that differ in their stopping params:
#' designs <- list(
#'   "BOIN12 v1" = get_boin12(num_doses = 5,
#'                            phi_t = 0.35, phi_e = 0.25,
#'                            u2 = 40, u3 = 60,
#'                            c_t = 0.95, c_e = 0.9) %>%
#'     stop_at_n(n = 36),
#'   "BOIN12 v2" = get_boin12(num_doses = 5,
#'                            phi_t = 0.35, phi_e = 0.25,
#'                            u2 = 40, u3 = 60,
#'                            c_t = 0.5, c_e = 0.5) %>%
#'     stop_at_n(n = 36)
#' )
#' # For illustration we run only 10 iterates:
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff
#' )
#' # To compare toxicity-only designs like CRM etc, we would omit true_prob_eff.
#'
#' # We might be interested in the absolute dose recommendation probabilities:
#' convergence_plot(x)
#'
#' library(dplyr)
#' library(ggplot2)
#' # and, perhaps more importantly, how they compare:
#' as_tibble(x) %>%
#'   ggplot(aes(x = n, y = delta)) +
#'   geom_point(size = 0.4) +
#'   geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
#'   geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
#'   facet_grid(comparison ~ dose,
#'     labeller = labeller(
#'       .rows = label_both,
#'       .cols = label_both)
#'   )
#'
#' # Simulations for each design are available by name:
#' sims <- x$`BOIN12 v1`
#' # And the usual functions are available on the sims objects:
#' sims %>% num_patients()
#' sims %>% num_doses()
#' sims %>% dose_indices()
#' sims %>% n_at_dose()
#' # etc
#' # See ? simulate_trials
#'
#' # As with simulate_trials, which examines one design, we also have options to
#' # tweak the simulation process.
#'
#' # By default, dose decisions are made after each cohort of 3 patients. To
#' # override, specify an alternative function via the sample_patient_arrivals
#' # parameter. E.g. to use cohorts of 2, we run:
#' patient_arrivals_func <- function(current_data) cohorts_of_n(n = 2)
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff,
#'   sample_patient_arrivals = patient_arrivals_func
#' )
#'
#' # To simulate the culmination of trials that are partly completed, specify
#' # the outcomes already observed via the previous_outcomes parameter. Imagine
#' # one cohort has already been evaluated, returning outcomes 1NTN. We can
#' # simulate the remaining part of that trial with:
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff,
#'   previous_outcomes = '1NTN'
#' )
#'
#' # Outcomes can be described by the above outcome string method or data-frame:
#' previous_outcomes <- data.frame(
#'   patient = 1:3,
#'   cohort = c(1, 1, 1),
#'   tox = c(0, 1, 0),
#'   eff = c(1, 1, 0),
#'   dose = c(1, 1, 1)
#' )
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff,
#'   previous_outcomes = previous_outcomes
#' )
#'
#' # We can specify the immediate next dose:
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff,
#'   next_dose = 5
#' )
#'
#' # By default, the method will stop simulated trials after 30 dose selections.
#' # To suppress this, specify i_like_big_trials = TRUE. However, please take
#' # care to specify selectors that will eventually stop! Our designs above use
#' # stop_at_n so they will not proceed ad infinitum.
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff,
#'   i_like_big_trials = TRUE
#' )
#'
#' # By default, only the final model fit is retained for each simulated trial.
#' # To retain all interim model fits, specify return_all_fits = TRUE.
#' x <- simulate_compare(
#'   designs,
#'   num_sims = 10,
#'   true_prob_tox,
#'   true_prob_eff,
#'   return_all_fits = TRUE
#' )
#' }
#'
#' @references
#' Sweeting, M., Slade, D., Jackson, D., Brock, K. (2023)
#' Potential outcome simulation for efficient head-to-head comparison of
#' adaptive dose-finding designs. Preprint.
simulate_compare <- function(
    designs,
    num_sims,
    true_prob_tox,
    true_prob_eff = NULL,
    patient_samples = NULL,
    rho = NULL,
    return_patient_samples = FALSE,
    ...) {

  if(is.null(patient_samples)) {
    if(!is.null(rho)) {
      # Correlated toxicity and efficacy events
      patient_samples <- lapply(
        1:num_sims,
        function(x) CorrelatedPatientSample$new(rho = rho)
      )
    } else {
      # Non-correlated toxicity and efficacy events
      patient_samples <- lapply(
        1:num_sims,
        function(x) PatientSample$new()
      )
    }
  } else {
    if(length(patient_samples) != num_sims) {
      stop("If specified, patient_samples should be a list of length num_sims")
    }

    if(!is.null(rho)) {
      stop("Specify rho or patient_samples (or neither) but not both.
        If you specify rho, this function will use instances of
        CorrelatedPatientSample to impart association between events.")
    }
  }

  out <- list()
  for(label in ls(designs)) {
    cat("Running", label, "\n")
    design <- designs[[label]]
    sim_func <- simulation_function(design)
    if(is.null(true_prob_eff)) {
      l <- lapply(
        1:num_sims,
        function(i) {
          sim_func(design,
                   true_prob_tox = true_prob_tox,
                   patient_sample = patient_samples[[i]],
                   ...)
        }
      )
    } else {
      l <- lapply(
        1:num_sims,
        function(i) {
          sim_func(design,
                   true_prob_tox = true_prob_tox,
                   true_prob_eff = true_prob_eff,
                   patient_sample = patient_samples[[i]],
                   ...)
        }
      )
    }

    sims <- simulations(
      fits = l,
      true_prob_tox = true_prob_tox,
      true_prob_eff = true_prob_eff
    )
    out[[label]] <- sims
  }

  if(return_patient_samples)
    attr(out, "patient_samples") <- patient_samples

  return(simulations_collection(out))
}
