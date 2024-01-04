
#' Get potential outcomes from a list of PatientSamples
#'
#' @description
#' An instance of \code{\link{PatientSample}}, or one of its subclasses like
#' \code{\link{CorrelatedPatientSample}}, reflects one particular state of the
#' world, where patient i would reliably experience a toxicity or efficacy
#' event if treated at a particular dose. This function, given true toxicity and
#' efficacy probabilities at doses 1, ..., num_doses, calculates 0/1 matrices to
#' reflect whether the patients in those samples would have experienced toxicity
#' and efficacy at the doses, had they been dosed as such. Using the
#' vernacular of causal inference, these are _potential outcomes_. At any single
#' instant, a patient can only be dosed at one dose, so only one of the
#' outcomes for a patient would in reality have been observed; the rest are
#' counterfactual.
#'
#' @param patient_samples list of \code{\link{PatientSample}} objects, or
#' subclass thereof.
#' @param true_prob_tox vector of probabilities of toxicity outcomes at doses
#' @param true_prob_eff vector of probabilities of efficacy outcomes at doses
#'
#' @return a list of lists, with names tox and eff, each mapping to a matrix of
#' the potential outcomes.
#'
#' @export
#'
#' @examples
#' num_sims <- 10
#' ps <- lapply(1:num_sims, function(x) PatientSample$new())
#' # Set tox_u and eff_u for each simulation
#' set.seed(2024)
#' lapply(1:num_sims, function(x) {
#'   tox_u_new <- runif(n = 20)
#'   eff_u_new <- runif(n = 20)
#'   ps[[x]]$set_eff_and_tox(tox_u = tox_u_new, eff_u = eff_u_new)
#' })
#' true_prob_tox <- c(0.05, 0.10, 0.15, 0.18, 0.45)
#' true_prob_eff <- c(0.40, 0.50, 0.52, 0.53, 0.53)
#' get_potential_outcomes(
#'   patient_samples = ps,
#'   true_prob_tox = true_prob_tox,
#'   true_prob_eff = true_prob_eff
#' )
get_potential_outcomes = function(patient_samples,
                                  true_prob_tox,
                                  true_prob_eff) {
  if (length(true_prob_tox) != length(true_prob_eff))
    stop("true_prob_tox and true_prob_eff should have same length")
  ps <- patient_samples
  num_sims <- length(ps)
  num_doses <- length(true_prob_tox)
  lapply(1:num_sims, function(x) {
    # loop over PatientSamples
    tox <- sapply(seq_len(num_doses), function(d) {
      # loop over doses
      sapply(seq_len(ps[[x]]$num_patients), function(i) {
        # loop over patients
        ps[[x]]$get_patient_tox(i = i, prob_tox = true_prob_tox[d])
      })
    })

    # loop over PatientSamples
    eff <- sapply(seq_len(num_doses), function(d) {
      # loop over doses
      sapply(seq_len(ps[[x]]$num_patients), function(i) {
        # loop over patients
        ps[[x]]$get_patient_eff(i = i, prob_eff = true_prob_eff[d])
      })
    })

    list(tox = tox, eff = eff)
  })
}
