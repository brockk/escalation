
# Calculating prob_tox quantiles in BOIN is not as trivial as dipping into the
# posterior distribution, at least not if we want consistency between the
# quantile and the prob_tox_exceeds functions, because the authors use isotonic
# regression to ensure estimates that are non-decreasing in dose.
# Alors, we instead perform a little grid-search. It risks being slow but it
# ensures consistency between the complimentary functions.

library(escalation)

target <- 0.25
model1 <- get_boin(num_doses = 5, target = target)
outcomes <- '1NNN 2NTN 3NTN 4NTT 5NNT'
fit <- model1 %>% fit(outcomes)
class(fit)
fit %>% recommended_dose()
fit %>% mean_prob_tox()

selector <- fit
p <- 0.95

thresholds <- seq(0, 1, length.out = 101)
system.time({
  tibble::tibble(
    q = thresholds
  ) %>% dplyr::mutate(
    dose = purrr::map(q, .f = ~ dose_indices(selector)),
    prob = purrr::map(q, .f = ~ 1 - prob_tox_exceeds(selector, threshold = .x))
  ) %>% tidyr::unnest(cols = c(dose, prob)) %>%
    dplyr::group_by(dose) %>%
    dplyr::slice(which.min(abs(prob - p)))
})

# For comparison:
system.time({
  prob_tox_exceeds(selector, 0.15)
})


#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr mutate group_by slice ungroupselect
#' @export
prob_tox_quantile.boin_selector <- function(
  selector, p,
  quantile_candidates = seq(0, 1, length.out = 101),
  ...) {

  x <- tibble::tibble(
    q = quantile_candidates
  ) %>% dplyr::mutate(
    dose = purrr::map(q, .f = ~ dose_indices(selector)),
    prob = purrr::map(q, .f = ~ 1 - prob_tox_exceeds(selector, threshold = .x))
  ) %>% tidyr::unnest(cols = c(dose, prob)) %>%
    dplyr::group_by(dose) %>%
    dplyr::slice(which.min(abs(prob - p))) %>%
    dplyr::ungroup() %>%
    dplyr::select(q) %>% .[[1]]
  # names(x) <- dose_indices(selector)
  x
}

#' @export
median_prob_tox.boin_selector <- function(selector, ...) {
  prob_tox_quantile(selector, p = 0.5, ...)
}


prob_tox_quantile(selector, 0.5)
prob_tox_quantile(selector, 1 - p)
prob_tox_quantile(selector, p)
median_prob_tox(selector)
mean_prob_tox(selector)

prob_tox_quantile(selector, 0.5, quantile_candidates = seq(0, 1, length.out = 1001))
prob_tox_quantile(selector, 1 - p, quantile_candidates = seq(0, 1, length.out = 1001))
prob_tox_quantile(selector, p, quantile_candidates = seq(0, 1, length.out = 1001))
median_prob_tox(selector, quantile_candidates = seq(0, 1, length.out = 1001))
mean_prob_tox(selector)


#' prob_tox_quantile.boin_selector <- function(selector, p, ...) {
#'   message('Note that BOIN does not estimate prob_tox_quantile')
#'   as.numeric(rep(NA, num_doses(selector)))
#' }

#' median_prob_tox.boin_selector <- function(selector, ...) {
#'   message('Note that BOIN does not estimate median_prob_tox.')
#'   as.numeric(rep(NA, num_doses(selector)))
#' }
