
#' Weights for tolerance and toxicity events using linear function of time
#'
#' @param now_time the time now
#' @param recruited_time vector of recruitment times for patients
#' @param tox integer vector of toxicity variables for patients, 1 means tox
#' @param max_time the maximum window of evaluation for
#' @param tox_has_weight_1 logical, TRUE to set the weight for tox to 1
#' identically
#'
#' @return numerical vector of weights
#'
#' @export
#'
#' @examples
#' linear_follow_up_weight(
#'   now_time = 10,
#'   recruited_time = 4:7,
#'   tox = c(0, 0, 0, 1),
#'   max_time = 6,
#'   tox_has_weight_1 = TRUE
#' )
#'
#' linear_follow_up_weight(
#'   now_time = 10,
#'   recruited_time = 4:7,
#'   tox = c(0, 0, 0, 1),
#'   max_time = 6,
#'   tox_has_weight_1 = FALSE
#' )
linear_follow_up_weight <- function(now_time, recruited_time, tox,
                                    max_time, tox_has_weight_1 = TRUE) {
  if(max_time <= 0 | length(max_time) > 1) {
    stop("stop_time must be strictly positive scalar number")
  }
  fup_time <- pmax(now_time - recruited_time, 0)
  weight <- pmin(fup_time / max_time, 1)
  if(tox_has_weight_1) {
    weight[tox == 1] <- 1
  }
  return(weight)
}
