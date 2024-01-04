
#' Make an instance of type \code{simulations_collection}
#'
#' @description
#' This object can be cast to a tibble with \code{as_tibble} to generate useful
#' pairwise comparisons of the probability of recommending each dose for each
#' pair of designs investigated. See
#' \code{\link{as_tibble.simulations_collection}} for a description.
#'
#' @param sim_map list, character -> \code{\link{simulations}} object
#'
#' @return object of class \code{simulations_collection}, inheriting from list
#' @export
#'
#' @references
#' Sweeting, M., Slade, D., Jackson, D., & Brock, K. (2023).
#' Potential outcome simulation for efficient head-to-head comparison of
#' adaptive dose-finding designs. Preprint.
simulations_collection <- function(sim_map) {
  class(sim_map) <- c("simulations_collection", class(sim_map))
  return(sim_map)
}

#' Convert a simulations_collection to a tibble
#'
#' @description
#' Cumulative statistics are shown to gauge how the simulations converge.
#'
#' @param x object of type \code{\link{simulations_collection}}
#' @param target_dose numerical dose index, or NULL (default) for all doses
#' @param alpha significance level for symmetrical confidence intervals
#' @param ... extra args are ignored
#'
#' @return a tibble with cols:
#' \itemize{
#'  \item{"dose"}{the dose-level}
#'  \item{"n"}{cumulative inference using the first n simulated iterations}
#'  \item{"design.x"}{The first design in the comparison, aka design X}
#'  \item{"hit.x"}{logical showing if design X recommended dose in iterate n}
#'  \item{"design.y"}{The second design in the comparison, aka design Y}
#'  \item{"hit.x"}{logical showing if design Y recommended dose in iterate n}
#'  \item{"X"}{cumulative sum of hit.x within dose, i.e. count of recommendations}
#'  \item{"X2"}{cumulative sum of hit.x^2 within dose}
#'  \item{"Y"}{cumulative sum of hit.y within dose, i.e. count of recommendations}
#'  \item{"Y2"}{cumulative sum of hit.y^2 within dose}
#'  \item{"XY"}{cumulative sum of hit.x * hit.y within dose}
#'  \item{"psi1"}{X / n}
#'  \item{"psi2"}{Y / n}
#'  \item{"v_psi1"}{variance of psi1}
#'  \item{"v_psi2"}{variance of psi2}
#'  \item{"cov_psi12"}{covariance of psi1 and psi2}
#'  \item{"delta"}{psi1 - psi2}
#'  \item{"v_delta"}{variance of delta}
#'  \item{"se_delta"}{standard error of delta}
#'  \item{"delta_l"}{delta - q * se_delta, where q is alpha / 2 normal quantile}
#'  \item{"delta_u"}{delta + q * se_delta, where q is alpha / 2 normal quantile}
#'  \item{"comparison"}{Label of design.x vs design.y, using design names}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble inner_join select mutate filter group_by ungroup
#' @export
as_tibble.simulations_collection <- function(x, target_dose = NULL,
                                             alpha = 0.05,
                                             ...) {
  sim_map <- x
  q <- qnorm(p = alpha / 2, lower.tail = FALSE)
  stacked_df <- stack_sims_vert(sim_map = sim_map, target_dose = target_dose,
                                alpha = alpha)

  # Avoid NOTEs
  dose <- n <- design <- hit <- NULL
  design.x <- design.y <- hit.x <- hit.y <- NULL
  X <- Y <- X2 <- Y2 <- XY <- NULL
  psi1 <- psi2 <- v_psi1 <- v_psi2 <- cov_psi12 <- NULL
  v_delta <- delta <- se_delta <- NULL

  # Compare each design to every other design:
  inner_join(
    stacked_df %>%
      select(dose, n, design, hit) %>%
      mutate(design = ordered(design)),
    stacked_df %>%
      select(dose, n, design, hit) %>%
      mutate(design = ordered(design)),
    by = c("dose", "n"),
    relationship ="many-to-many"
  ) %>%
    # Discard self-comparisons and replicates:
    filter(design.x > design.y) %>%
    group_by(dose, design.x, design.y) %>%
    mutate(
      X = cumsum(hit.x),
      X2 = cumsum(hit.x^2),
      Y = cumsum(hit.y),
      Y2 = cumsum(hit.y^2),
      XY = cumsum(hit.x * hit.y),
      psi1 = X / n,
      psi2 = Y / n,
      v_psi1 = (X2 / n - (X / n)^2) / n,
      v_psi2 = (Y2 / n - (Y / n)^2) / n,
      cov_psi12 = (XY / n - (X / n) * (Y / n)) / n,
      delta = psi1 - psi2,
      v_delta = v_psi1 + v_psi2 - 2 * cov_psi12,
      se_delta = sqrt(v_delta),
      delta_l = delta - q * se_delta,
      delta_u = delta + q * se_delta,
      comparison = paste0(design.x, " vs ", design.y)
    ) %>%
    ungroup() %>%
    as_tibble(...)
}
