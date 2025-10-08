
#' Tabulate rank-based desirability scores for a BOIN12 trial
#'
#' @inheritParams get_boin12
#' @param sample_sizes integer vector, cohort sample sizes to consider
#'
#' @return data.frame with columns Patients, Toxicity, Efficacy containing the
#' numbers of patients, patients with toxicitiy, and patients with efficacy;
#' Admissble, containing the character labels Admissble or Not Admissible;
#' RDS, containing a character label of the numerical desirability score or the
#' character string "E", where a combination is eliminated;
#' and RDS_x, containing the desirability scores as numbers, with NA where a
#' combination should be eliminated.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble bind_rows mutate rename select
#' @importFrom stats pbeta
#' @export
#'
#' @author Bharat Bhushan, Kristian Brock
#'
#' @references
#' Lin, R., Zhou, Y., Yan, F., Li, D., & Yuan, Y. (2020).
#' BOIN12: Bayesian optimal interval phase I/II trial design for utility-based
#' dose finding in immunotherapy and targeted therapies.
#' JCO Precision Oncology, 4, 1393-1402.
#'
#' @examples
#' # Table 3 in Lin et al.
#' x <- boin12_rds(
#'   sample_sizes = c(0, 3, 6, 9),
#'   phi_t = 0.35,
#'   phi_e = 0.25,
#'   u1 = 100,
#'   u2 = 40,
#'   u3 = 60,
#'   u4 = 0,
#'   c_t = 0.95,
#'   c_e = 0.9,
#'   prior_alpha = 1,
#'   prior_beta = 1
#' )
boin12_rds <- function(
    # max_num_patients,
    sample_sizes,
    phi_t,
    phi_e,
    u1 = 100,
    u2,
    u3,
    u4 = 0,
    c_t = 0.95,
    c_e = 0.9,
    prior_alpha = 1,
    prior_beta = 1
) {

  poss <- data.frame()
  # Loop over the number of patients
  for (i in sample_sizes) {
  # for (i in seq(0, max_num_patients)) {
    # Loop over the number of efficacy events
    for (j in seq(0, i)) {
      # Loop over the number of toxicity events
      for (k in seq(0, i)) {
        # Append a new row
        poss <- bind_rows(poss, tibble(i, j, k))
      }
    }
  }
  poss <- tibble(poss)
  colnames(poss) <- c("target_toxs", "Tox", "Eff")

  u_bar <- u1 * (1 - phi_t) * phi_e +
    (u2 * (1 - phi_t) * (1 - phi_e) ) +
    (u3 * phi_t * phi_e)
  ub <- u_bar + (100 - u_bar) / 2

  # Avoid built NOTEs etc
  safety_prob <- efficacy_prob <- Admissible <- xd <- alpha <- beta <- NULL
  prob <- RDS <- RDS_x <- Patients <- Toxicity <- Efficacy <- Tox <- Eff <- NULL
  target_toxs <- NULL

  # Calculate p(u > ub) and RDS
  poss1 <- poss %>%
    mutate(
      safety_prob = pbeta(
        phi_t,
        prior_alpha + Tox,
        prior_beta + target_toxs -Tox,
        lower.tail = FALSE
      ),
      efficacy_prob = pbeta(
        phi_e,
        prior_alpha + Eff,
        prior_beta + target_toxs - Eff,
        lower.tail = TRUE
      ),
      Admissible = ifelse(
        safety_prob < c_t & efficacy_prob < c_e,
        "Admissible",
        "Not Admissible"
      ),
      xd = ((u3 * Eff) + u2 * (target_toxs - Tox)) / 100,
      alpha = prior_alpha + xd,
      beta = (prior_beta + target_toxs - xd),
      prob = pbeta(ub / 100, alpha, beta, lower.tail = FALSE)
    ) %>%
    mutate(
      # Rank prob among admissible only:
      RDS = ifelse(
        Admissible == "Admissible",
        rank(ifelse(Admissible == "Admissible", prob, NA)),
        "E"
      ),
      RDS_x = ifelse(
        Admissible == "Admissible",
        rank(ifelse(Admissible == "Admissible", prob, NA)),
        NA
      )
    )

  # Final RDS table
  out <- poss1 %>%
    rename(Patients = target_toxs, Toxicity = Tox, Efficacy = Eff) %>%
    select(Patients, Toxicity, Efficacy, Admissible, RDS, RDS_x)
  return(out)
}
