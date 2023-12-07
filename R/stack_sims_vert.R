
#' Stack \code{\link{simulations_collection}} results vertically
#'
#' @param sim_map object of type \code{\link{simulations_collection}}
#' @param target_dose optional integer vector, the dose of interest. All doses
#'          are analysed if omitted, which is the default.
#' @param alpha confidence level for asymptotic normal confidence intervals. The
#' default value is 0.05 to get 95 percent confidence intervals.
#'
#' @return a data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map imap reduce
#' @importFrom dplyr tibble bind_rows mutate
#' @export
#'
#' @examples
#' # In a five-dose scenario, we have assumed probabilities for Prob(tox):
#' true_prob_tox <- c(0.05, 0.10, 0.15, 0.18, 0.45)
#' # and Prov(eff):
#' true_prob_eff <- c(0.40, 0.50, 0.52, 0.53, 0.53)
#'
#' # Let us compare two BOIN12 variants that differ in their stopping params:
#' designs <- list(
#'   "BOIN12 v1" = get_boin12(num_doses = num_doses,
#'                            phi_t = 0.35, phi_e = 0.25,
#'                            u2 = 40, u3 = 60,
#'                            c_t = 0.95, c_e = 0.9) %>%
#'     stop_at_n(n = 36),
#'   "BOIN12 v2" = get_boin12(num_doses = num_doses,
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
#' stack_sims_vert(x)
stack_sims_vert <- function(sim_map, target_dose = NULL, alpha = 0.05) {
  q <- qnorm(p = alpha / 2, lower.tail = FALSE)
  sim_map %>%
    imap(
      ~ {
        if(is.null(target_dose)) target_dose <- dose_indices(.x)
        rec_d <- recommended_dose(.x)
        rec_d[is.na(rec_d)] <- 0 # Replace NAs with 0

        target_dose %>%
          map(.f = function(td) {
            tibble(
              dose = td,
              hit = rec_d == td,
              r = cumsum(hit),
              n = seq_len(length(.x))
            )
          }) %>%
          reduce(bind_rows) %>%
          mutate(design = .y)

      }
    ) %>%
    reduce(bind_rows) %>%
    mutate(
      .rate = r / n,
      .se = sqrt(.rate * (1 - .rate) / n),
      .l = .rate - q * .se,
      .u = .rate + q * .se
    )
}
