
#' @title A sample of patients that experience correlated events in simulations.
#'
#' @description
#' Class to house the latent random variables that govern toxicity and efficacy
#' events in patients. Instances of this class can be used in simulation-like
#' tasks to effectively use the same simulated individuals in different designs,
#' thus supporting reduced Monte Carlo error and more efficient comparison. This
#' class differs from \code{\link{PatientSample}} in that the latent variables
#' that underlie efficacy and toxicity events, and therefore those events
#' themselves, are correlated, e.g. for positive association, a patient that
#' experiences toxicity has increased probability of experiencing efficacy too.
#' Correlated uniformly-distributed variables are obtained by inverting
#' bivariate normal variables. The extent to which the events are correlated is
#' controlled by rho, the correlation of the two normal variables.
#'
#' @importFrom R6 R6Class
#' @export
#'
#' @references
#' Sweeting, M., Slade, D., Jackson, D., & Brock, K. (2024).
#' Potential outcome simulation for efficient head-to-head comparison of
#' adaptive dose-finding designs. arXiv preprint arXiv:2402.15460
CorrelatedPatientSample <- R6Class("CorrelatedPatientSample",
  inherit = PatientSample,
  public = list(

    #' @field num_patients (`integer(1)`)\cr
    num_patients = NULL,
    #' @field mu (`numeric(2)`)\cr
    mu = NULL,
    #' @field sigma (`matrix(2, 2)`)\cr
    sigma = NULL,

    #' @description
    #' Creator.
    #'
    #' @param num_patients (`integer(1)`) Number of patients.
    #' @param time_to_tox_func (`function`) function taking no args that returns
    #' a single time of toxicity, given that toxicity occurs.
    #' @param time_to_eff_func (`function`) function taking no args that returns
    #' a single time of efficacy, given that efficacy occurs.
    #' @param rho (`integer(1)`) correlation of toxicity and efficacy latent
    #' variables.
    #'
    #' @return [CorrelatedPatientSample].
    initialize = function(
      num_patients = 0,
      time_to_tox_func = function() runif(n = 1),
      time_to_eff_func = function() runif(n = 1),
      rho = 0
    ) {
      self$mu <- rep(0, 2)
      self$sigma <- matrix(c(1, rho, rho, 1), ncol = 2)

      super$initialize(
        num_patients = num_patients,
        time_to_tox_func = time_to_tox_func,
        time_to_eff_func = time_to_eff_func
      )
    },

    #' @description
    #' Expand sample to size at least num_patients
    #' @param num_patients (`integer(1)`).
    #' @importFrom mvtnorm rmvnorm
    #' @importFrom stats pnorm
    expand_to = function(num_patients) {
      if(num_patients > self$num_patients) {
        if(self$can_grow) {
          z <- rmvnorm(
            n = num_patients - self$num_patients,
            mean = self$mu,
            sigma = self$sigma
          )
          new_tox_u <- pnorm(z[, 1])
          self$tox_u <- c(self$tox_u, new_tox_u)
          new_tox_time <- map_dbl(
            seq_len(num_patients),
            ~ self$time_to_tox_func()
          )
          self$tox_time <- c(self$tox_time, new_tox_time)
          new_eff_u <- pnorm(z[, 2])
          self$eff_u <- c(self$eff_u, new_eff_u)
          new_eff_time <- map_dbl(
            seq_len(num_patients),
            ~ self$time_to_eff_func()
          )
          self$eff_time <- c(self$eff_time, new_eff_time)

          self$num_patients <- num_patients
        } else {
          stop("Attempt to grow a fixed patient sample")
        }
      }
    }

  )
)
