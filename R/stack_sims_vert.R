
#' Stack \code{\link{simulations_collection}} results vertically
#'
#' @param sim_map object of type \code{\link{simulations_collection}}
#' @param target_dose optional integer vector, the dose of interest. All doses
#'          are analysed if omitted, which is the default.
#' @param alpha confidence level for asymptotic normal confidence intervals. The
#' default value is 0.05 to get 95% confidence intervals.
#'
#' @return \code{data.frame}
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map imap reduce
#' @importFrom dplyr tibble bind_rows mutate
#' @export
#'
#' @examples
#' # TODO
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
