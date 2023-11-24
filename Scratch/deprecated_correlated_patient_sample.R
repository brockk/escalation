
#' @title A sample of patients that experience correlated events in simulations.
#'
#' @description
#' Class to house the latent random variables that govern toxicity and efficacy
#' events in patients. Instances of this class can be used in simulation-like
#' tasks to effectively use the same simulated individuals in different designs,
#' thus supporting reduced Monte Carlo error and more efficient comparison. This
#' class differs from \code{\link{PatientSample}} in that the latent variables
#' that underly efficacy and toxicity events, and therefore those events
#' themselves, are correlated, e.g. for positive association, a patient that
#' experiences toxicity has increased probability of experiencing efficacy too.
#' The extent to which the events are correlated is controlled by tau. TODO
#'
#' @importFrom R6 R6Class
#' @export
#'
#' @references
#' Sweeting, M., Slade, D., Jackson, D., & Brock, K. (2023).
#' Potential outcome simulation for efficient head-to-head comparison of
#' adaptive dose-finding designs. Preprint.
CorrelatedPatientSample <- R6Class("CorrelatedPatientSample",
  inherit = PatientSample,
  public = list(

    #' @field num_patients (`integer(1)`)\cr
    num_patients = NULL,
    #' @field tau (`numeric(1)`)\cr
    tau = NULL,
    #' @field prob_event (`numeric(41)`)\cr
    prob_event = NULL,
    #' @field xi (`numeric(41)`)\cr
    xi = NULL,
    #' @field theta (`numeric(num_patients)`)\cr
    theta = NULL,
    #' @field can_grow (`logical(1)`)\cr
    can_grow = NULL,

    #' @description
    #' Creator.
    #'
    #' @param num_patients (`integer(1)`).
    #' @param tau (`integer(1)`) standard deviation of normal latent variable
    #' @return [PatientSample].
    #' @importFrom stats rnorm uniroot
    #' @importFrom purrr map_dbl
    #' @importFrom magrittr %>%
    initialize = function(num_patients = 0, tau = 1) {

      # Define prob_event vs xi grid:
      self$tau <- tau
      theta <- rnorm(n = 10^6, mean = 0, sd = tau)
      xi_grid <- seq(-10, 10, 0.5)
      self$prob_event <- expit(xi_grid)
      self$xi <- seq_along(self$prob_event) %>%
        map_dbl(~{
          y <- uniroot(
            function(x) mean(expit(x + theta)) - self$prob_event[.x],
            interval = c(-10^2, 10^2)
          )
          y$root
        })

      # Now initialise for num_patients
      self$num_patients <- num_patients
      self$theta <- rnorm(n = num_patients, mean = 0, sd = tau)
      self$can_grow <- TRUE
    },

    #' @description
    #' Set the theta latent variables that govern occurrence of
    #' toxicity and efficacy events. By default, instances of this class
    #' automatically grow these latent variables to accommodate arbitrarily high
    #' sample sizes. However, when you set these latent variables manually via
    #' this function, you override the ability of the class to self-manage, so
    #' its ability to grow is turned off by setting the internal variable
    #' \code{self$can_grow <- FALSE}.
    #'
    #' @param theta (`numeric()`).
    set_theta = function(theta) {
      self$num_patients <- length(theta)
      self$theta <- theta
      self$can_grow <- FALSE
    },

    #' @description
    #' Expand sample to size at least num_patients
    #'
    #' @param num_patients (`integer(1)`).
    expand_to = function(num_patients) {
      if(num_patients > self$num_patients) {
        if(self$can_grow) {
          self$theta <- c(
            self$theta,
            rnorm(n = num_patients - self$num_patients, mean = 0, sd = self$tau)
          )
          self$num_patients <- num_patients
        } else {
          stop("Attempt to grow a fixed patient sample")
        }
      }
    },

    #' @description
    #' Get theta latent variable for patient i
    #'
    #' @param i (`integer(1)`) patient index
    get_theta = function(i) {
      if(i > self$num_patients) {
        self$expand_to(num_patients = i)
      }
      return(self$theta[i])
    },

    #' @description
    #' Get 0 or 1 event marker for whether toxicity occurred in patient i
    #'
    #' @param i (`integer(1)`) patient index
    #' @param prob_tox (`numeric(1)`) probability of toxicity
    get_patient_tox = function(i, prob_tox) {
      if(length(prob_tox) > 1) {
        stop("prob_tox should be of length one")
      }
      if(prob_tox >= 1) return(rep(1, length(i)))
      if(prob_tox <= 0) return(rep(0, length(i)))
      if(length(i) > 1) {
        theta <- sapply(i, function(x) self$get_theta(x))
      } else {
        theta <- self$get_theta(i)
      }
      # Interpolate xi for prob_tox
      this_xi <- approx(x = self$prob_event, y = self$xi, xout = prob_tox)
      return(as.integer(expit(this_xi$y + theta) > prob_tox))
    },

    #' @description
    #' Get 0 or 1 event marker for whether efficacy occurred in patient i
    #'
    #' @param i (`integer(1)`) patient index
    #' @param prob_eff (`numeric(1)`) probability of efficacy
    get_patient_eff = function(i, prob_eff) {
      if(length(prob_eff) > 1) {
        stop("prob_eff should be of length one")
      }
      if(prob_eff >= 1) return(rep(1, length(i)))
      if(prob_eff <= 0) return(rep(0, length(i)))
      if(length(i) > 1) {
        theta <- sapply(i, function(x) self$get_theta(x))
      } else {
        theta <- self$get_theta(i)
      }
      # Interpolate xi for prob_eff
      this_xi <- approx(x = self$prob_event, y = self$xi, xout = prob_eff)
      return(as.integer(expit(this_xi$y + theta) > prob_eff))
    }
  )
)
