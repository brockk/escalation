
#' @title Patient tolerance samples for simulations
#'
#' @description
#' Class to house the latent random variables that govern tox and eff events in
#' simulations.
#'
#' @param integer number of patients
#'
#' @importFrom R6 R6Class
#' @export
PatientSample <- R6Class(
  "PatientSample",
  public = list(

    #' @field num_patients (`integer(1)`)\cr
    num_patients = NULL,
    #' @field tox_u (`numeric(num_patients)`)\cr
    tox_u = NULL,
    #' @field eff_u (`numeric(num_patients)`)\cr
    eff_u = NULL,

    #' @description
    #' Creator.
    #'
    #' @param num_patients (`integer(1)`).
    #' @return [PatientSample].
    initialize = function(num_patients = 0) {
      self$tox_u <- runif(n = num_patients)
      self$eff_u <- runif(n = num_patients)
      self$num_patients <- num_patients
    },

    #' @description
    #' Expand sample to size at least num_patients
    #'
    #' @param num_patients (`integer(1)`).
    expand_to = function(num_patients) {
      if(num_patients > self$num_patients) {
        self$tox_u <- c(
          self$tox_u,
          runif(n = num_patients - self$num_patients)
        )
        self$eff_u <- c(
          self$eff_u,
          runif(n = num_patients - self$num_patients)
        )
        self$num_patients <- num_patients
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
