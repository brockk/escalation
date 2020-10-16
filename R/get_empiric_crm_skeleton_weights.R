

#' Get posterior model weights for several empiric CRM skeletons.
#'
#' Get posterior model weights for several empiric CRM skeletons, assuming a
#' normal prior on the beta model parameter
#'
#' @param skeletons matrix with one skeleton per row, so that the number of
#' columns is the number of doses under investigation.
#' @param events_at_dose integer vector of number of events at doses
#' @param n_at_dose integer vector of number of patients at doses
#' @param prior vector of prior model weights. Length should be same as number
#' of rows in \code{skeletons}. Default is equal weighting.
#'
#' @return numerical vector, posterior weights of the skeletons.
#'
#' @export
#' @importFrom stats integrate
#'
#' @examples
#' # TODO
get_empiric_crm_skeleton_weights <- function(
  skeletons,
  events_at_dose,
  n_at_dose,
  prior = rep(1, nrow(skeletons))) {

  if(nrow(skeletons) <= 0) {
    stop(paste0('Number of rows in skeletons must be at least 1.'))
  }
  if(ncol(skeletons) != length(n_at_dose)) {
    stop(paste0('Number of columns in skeletons must equal length of ',
                'n_at_dose'))
  }
  if(length(events_at_dose) != length(n_at_dose)) {
    stop(paste0('Length of n_at_dose must equal length of ',
                'events_at_dose'))
  }
  if(nrow(skeletons) != length(prior)) {
    stop(paste0('Number of rows in skeletons must equal length of ',
                'prior'))
  }

  .integrand <- function(b, skeleton, events_at_dose, n_at_dose,
                         beta_sd = sqrt(1.34)) {
    lik <- 1
    for(i in 1:length(skeleton)){
      pi = skeleton[i]^exp(b)
      lik = lik * pi^events_at_dose[i] *
        (1 - pi)^(n_at_dose[i] - events_at_dose[i])
    }
    return(lik * exp(-0.5 * b * b / beta_sd^2));
  }

  marginal_lik <- numeric(length = length(n_at_dose))
  for(i in 1:nrow(skeletons)) {
    integral <- integrate(.integrand, lower = -Inf, upper = Inf,
                          skeleton = skeletons[i,],
                          events_at_dose = events_at_dose,
                          n_at_dose = n_at_dose)
    marginal_lik[i] <- integral$value
  }
  post_weights <- (marginal_lik * prior) / sum(marginal_lik * prior)
  post_weights
}
