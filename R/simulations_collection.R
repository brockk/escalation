
#' Make an instance of type \code{simulations_collection}
#'
#' @param sim_map list, character -> \class{\link{simulations}} object
#'
#' @return object of class \code{simulations_collection}, inheriting from list
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#'
#' @references
#' Sweeting, M., Slade, D., Jackson, D., & Brock, K. (2023).
#' Potential outcome simulation for efficient head-to-head comparison of
#' adaptive dose-finding designs. Preprint.
make_simulations_collection <- function(sim_map) {
  class(sim_map) <- c("simulations_collection", class(sim_map))
  return(sim_map)
}

#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble inner_join select mutate filter group_by ungroup
#' @export
as_tibble.simulations_collection <- function(x, target_dose = NULL,
                                             alpha = 0.05) {
  sim_map <- x
  q <- qnorm(p = alpha / 2, lower.tail = FALSE)
  stacked_df <- stack_sims_vert(sim_map, target_dose, alpha = alpha)
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
    group_by(dose) %>%
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
    ungroup()
}
