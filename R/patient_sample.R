
#' @title A sample of patients to use in simulations.
#'
#' @description
#' Class to house the latent random variables that govern toxicity and efficacy
#' events in patients. Instances of this class can be used in simulation-like
#' tasks to effectively use the same simulated individuals in different designs,
#' thus supporting reduced Monte Carlo error and more efficient comparison.
#'
#' @importFrom R6 R6Class
#' @export
#'
#' @references
#' Sweeting, M., Slade, D., Jackson, D., & Brock, K. (2024).
#' Potential outcome simulation for efficient head-to-head comparison of
#' adaptive dose-finding designs. arXiv preprint arXiv:2402.15460
PatientSample <- R6Class(
  "PatientSample",
  public = list(

    #' @field num_patients (`integer(1)`)\cr
    num_patients = NULL,
    #' @field tox_u (`numeric(num_patients)`)\cr
    tox_u = NULL,
    #' @field eff_u (`numeric(num_patients)`)\cr
    eff_u = NULL,
    #' @field can_grow (`logical(1)`)\cr
    can_grow = NULL,

    #' @description
    #' Creator.
    #'
    #' @param num_patients (`integer(1)`).
    #' @return [PatientSample].
    initialize = function(num_patients = 0) {
      self$num_patients <- 0
      self$tox_u <- numeric(length = 0)
      self$eff_u <- numeric(length = 0)
      self$can_grow <- TRUE

      self$expand_to(num_patients)
    },

    #' @description
    #' Set the toxicity and efficacy latent variables that govern occurrence of
    #' toxicity and efficacy events. By default, instances of this class
    #' automatically grow these latent variables to accommodate arbitrarily high
    #' sample sizes. However, when you set these latent variables manually via
    #' this function, you override the ability of the class to self-manage, so
    #' its ability to grow is turned off by setting the internal variable
    #' \code{self$can_grow <- FALSE}.
    #'
    #' @param tox_u (`numeric()`).
    #' @param eff_u (`numeric()`).
    set_eff_and_tox = function(tox_u, eff_u) {
      if(length(tox_u) == length(eff_u)) {
        self$num_patients <- length(eff_u)
        self$tox_u <- tox_u
        self$eff_u <- eff_u
        self$can_grow <- FALSE
      } else {
        stop("tox_u and eff_u must have same length")
      }
    },

    #' @description
    #' Expand sample to size at least num_patients
    #'
    #' @param num_patients (`integer(1)`).
    #' @importFrom stats runif
    expand_to = function(num_patients) {
      if(num_patients > self$num_patients) {
        if(self$can_grow) {
          self$tox_u <- c(
            self$tox_u,
            runif(n = num_patients - self$num_patients)
          )
          self$eff_u <- c(
            self$eff_u,
            runif(n = num_patients - self$num_patients)
          )
          self$num_patients <- num_patients
        } else {
          stop("Attempt to grow a fixed patient sample")
        }
      }
    },

    #' @description
    #' Get toxicity latent variable for patient i
    #'
    #' @param i (`integer(1)`) patient index
    get_tox_u = function(i) {
      if(i > self$num_patients) {
        self$expand_to(num_patients = i)
      }
      return(self$tox_u[i])
    },

    #' @description
    #' Get 0 or 1 event marker for whether toxicity occurred in patient i
    #'
    #' @param i (`integer(1)`) patient index
    #' @param prob_tox (`numeric(1)`) probability of toxicity
    get_patient_tox = function(i, prob_tox) {
      if(length(i) > 1) {
        u <- sapply(i, function(x) self$get_tox_u(x))
      } else {
        u <- self$get_tox_u(i)
      }
      return(as.integer(u < prob_tox))
    },

    #' @description
    #' Get efficacy latent variable for patient i
    #'
    #' @param i (`integer(1)`) patient index
    get_eff_u = function(i) {
      if(i > self$num_patients) {
        self$expand_to(num_patients = i)
      }
      return(self$eff_u[i])
    },

    #' @description
    #' Get 0 or 1 event marker for whether efficacy occurred in patient i
    #'
    #' @param i (`integer(1)`) patient index
    #' @param prob_eff (`numeric(1)`) probability of efficacy
    get_patient_eff = function(i, prob_eff) {
      if(length(i) > 1) {
        u <- sapply(i, function(x) self$get_eff_u(x))
      } else {
        u <- self$get_eff_u(i)
      }
      return(as.integer(u < prob_eff))
    }

  )
)
