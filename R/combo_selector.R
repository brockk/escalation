
#' Dose selector for combinations of treatments
#'
#' @description TODO
#'
#' @seealso \code{\link{selector}}
#' #' @export
#'
#' @examples
#' # TODO
combo_selector <- function() {
  # This function exists only to document the abstract class "combo_selector".
}

# # @export
# # @importFrom tibble tibble
# # @importFrom purrr map_chr
# model_frame.combo_selector <- function(x, ...) {
#
#   if(num_patients(x) > 0) {
#     dg <- doses_given(x)
#     dg_str <- map_chr(dg, dose_vector_to_string)
#     tibble(
#       patient = seq(1, num_patients(x)),
#       cohort = cohort(x) %>% as.integer(),
#       dose = doses_given(x) %>% as.integer(),
#       dose_string = dg_str,
#       tox = tox(x) %>% as.integer()
#     )
#   } else {
#     tibble(
#       patient = integer(length = 0),
#       cohort = integer(length = 0),
#       dose = integer(length = 0),
#       dose_string = character(length = 0),
#       tox = integer(length = 0)
#     )
#   }
# }

#' @export
dose_indices.combo_selector <- function(x, ...) {
  return(get_dose_combo_indices(num_doses(x)))
}

#' @export
#' @importFrom purrr map_chr
dose_strings.combo_selector <- function(x, ...) {
  d_str <- map_chr(dose_indices(x), dose_vector_to_string)
  return(d_str)
}

#' @importFrom purrr map_chr map_int
#' @export
n_at_dose.combo_selector <- function(x, dose = NULL, ...) {
  if(is.null(dose)) {
    # Matrix output of all doses by default:
    z <- .outcomes_to_arrays(df = model_frame(x), num_doses = num_doses(x))
    return(z$num_patients)
  } else if(any(is.na(dose))) {
    return(NA)
  } else if(is.character(dose)) {
    if(dose == 'recommended') {
      rec_d <- recommended_dose(x)
      rec_d_str <- dose_vector_to_string(rec_d)
      d_g_str <- doses_given(x, dose_string = TRUE)
      return(sum(d_g_str == rec_d_str))
    }
  } else {
    d_str <- dose_vector_to_string(dose)
    d_g_str <- doses_given(x, dose_string = TRUE)
    return(sum(d_g_str == d_str))
  }
}

#' @export
n_at_recommended_dose.combo_selector <- function(x, ...) {
  # rec_d <- recommended_dose(x)
  # if(is.na(rec_d)) {
  #   return(NA)
  # }
  # else {
  #   rec_d_str <- dose_vector_to_string(rec_d)
  #   n_at_d <- n_at_dose(x, dose = NULL)
  #   d_str <- dose_strings(x)
  #   return(n_at_d[d_str == rec_d_str])
  # }
  return(n_at_dose(x, dose = "recommended", ...))
}

#' @export
prob_administer.combo_selector <- function(x, ...) {
  # n_doses <- num_doses(x)
  # n_d <- n_at_dose(x)
  # names(n_d) <- 1:n_doses
  # n_d / sum(n_d)
  z <- .outcomes_to_arrays(df = model_frame(x), num_doses = num_doses(x))
  N <- sum(z$num_patients)
  if(N > 0) {
    return(z$num_patients / N)
  } else {
    return(array(0, dim = dim(z$num_patients)))
  }
}

#' @export
tox_at_dose.combo_selector <- function(x, ...) {

  # # Vector:
  # return(x$df_c$tox)

  # Matrix:
  z <- .outcomes_to_arrays(df = model_frame(x), num_doses = num_doses(x))
  return(z$num_tox)

}

#' @export
eff_at_dose.combo_selector <- function(x, ...) {

  # # Vector:
  # return(x$df_c$eff)

  # Matrix:
  z <- .outcomes_to_arrays(df = model_frame(x), num_doses = num_doses(x))
  return(z$num_eff)

}
