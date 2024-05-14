
#' Check the consistency of a dose_selector instance
#'
#' @param x dose_selector
#'
#' @return NULL
#'
#' @importFrom magrittr %>%
#' @importFrom testthat expect_equal
#' @importFrom dplyr slice filter pull
#' @export
#'
#' @examples
#' boin_fitter <- get_boin(num_doses = 5, target = 0.3)
#' x <- fit(boin_fitter, "1NNN")
#' check_dose_selector_consistency(x)
check_dose_selector_consistency <- function(x) {

  # Avoid NOTEs:
  recommended <- dose <- NULL

  x_df <- summary(x) %>% slice(-1) # drop the NoDose line
  d <- recommended_dose(x)

  expect_equal(
    x_df$dose %>% as.character() %>% as.integer(),
    dose_indices(x)
  )
  expect_equal(
    x_df$tox,
    tox_at_dose(x)
  )
  expect_equal(
    x_df$n,
    n_at_dose(x)
  )
  expect_equal(
    x_df$empiric_tox_rate,
    empiric_tox_rate(x),
    tolerance = 0.01
  )
  expect_equal(
    x_df$mean_prob_tox,
    mean_prob_tox(x),
    tolerance = 0.01
  )
  expect_equal(
    x_df$median_prob_tox,
    median_prob_tox(x),
    tolerance = 0.01
  )
  expect_equal(
    dose_admissible(x),
    x_df$admissible
  )
  if(!is.na(d)) {
    expect_equal(
      x_df %>%
        filter(recommended) %>% pull(dose) %>%
        as.character() %>% as.integer(),
      d
    )
  }
}
