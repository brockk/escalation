
#' Number of nodes in dose-paths analysis
#'
#' Number of possible nodes in an exhaustive analysis of dose-paths in a
#' dose-finding trial. The number of nodes at depth i is the the number of nodes
#' at depth i-1 multiplied by the number of possible cohort outcomes at depth i.
#' For instance, if there were 16 nodes at the previous depth and four possible
#' cohort outcomes at the current depth, then there are 64 possible nodes at the
#' current depth. Knowing the number of nodes in a dose-paths analysis helps the
#' analyst decide whether simulation or dose-paths are a better tool for
#' assessing operating characteristics of a dose-finding design.
#'
#' @param num_patient_outcomes integer, number of distinct possible outcomes for
#' each single patient
#' @param cohort_sizes integer vector of cohort sizes
#'
#' @return integer vector, number of nodes at increasing depths. The total
#' number of nodes is the sum of this vector.
#'
#' @importFrom purrr map_int
#' @export
#'
#' @examples
#' # In a 3+3 design, there are two possible outcomes for each patient and
#' # patients are evaluated in cohorts of three. In an analysis of dose-paths in
#' # the first two cohorts of three, how many nodes are there?
#' num_dose_path_nodes(num_patient_outcomes = 2, cohort_sizes = rep(3, 2))
#' # In contrast, using an EffTox design there are four possible outcomes for
#' # each patient. In a similar analysis of dose-paths in the first two cohorts
#' # of three, how many nodes are there now?
#' num_dose_path_nodes(num_patient_outcomes = 4, cohort_sizes = rep(3, 2))
num_dose_path_nodes <- function(num_patient_outcomes, cohort_sizes) {
  n_cohort_outcomes <- map_int(
    cohort_sizes,
    ~ num_cohort_outcomes(num_patient_outcomes = num_patient_outcomes,
                          cohort_size = .x)
  )
  cumprod(c(1, n_cohort_outcomes)) %>% as.integer()
}
