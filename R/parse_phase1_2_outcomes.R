
#' @title Parse a string of phase I/II dose-finding outcomes to vector notation.
#'
#' @description Parse a string of phase I/II dose-finding outcomes to a binary
#' vector notation necessary for model invocation.
#'
#' The outcome string describes the doses given, outcomes observed and groups
#' patients into cohorts. The format of the string is described in Brock et al.
#' (2017). See Examples.
#'
#' The letters E, T, N and B are used to represents patients that
#' experienced (E)fficacy only, (T)oxicity only, (B)oth efficacy and toxicity,
#' and (N)either. These letters are concatenated after numerical dose-levels to
#' convey the outcomes of cohorts of patients. For instance, \code{2ETB}
#' represents a cohort of three patients that were treated at dose-level 2, and
#' experienced efficacy, toxicity and both events, respectively. The results of
#' cohorts are separated by spaces. Thus, \code{2ETB 1NN} extends our previous
#' example, where the next cohort of two were treated at dose-level 1 and both
#' patients experienced neither efficacy nor toxicity. See Examples.
#'
#' @param outcomes character string, conveying doses given and outcomes
#' observed.
#' @param as_list TRUE (the default) to return a \code{list};
#' FALSE to return a \code{data.frame}
#'
#' @return If \code{as_list == TRUE}, a list with elements \code{eff},
#' \code{tox}, \code{dose} and \code{num_patients}. If \code{as_list == FALSE},
#' a data.frame with columns \code{eff}, \code{tox} and \code{dose}.
#'
#' @export
#'
#' @examples
#' x = parse_phase1_2_outcomes('1NNE 2EEN 3TBB')
#' # Three cohorts of three patients. The first cohort was treated at dose 1 and
#' # had no toxicity with one efficacy, etc.
#' x$num_patients  # 9
#' x$dose         # c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' x$eff           # c(0, 0, 1, 1, 1, 0, 0, 1, 1)
#' sum(x$eff)      # 5
#' x$tox           # c(0, 0, 0, 0, 0, 0, 1, 1, 1)
#' sum(x$tox)      # 3
#'
#' # The same information can be parsed to a data-frame:
#' y = parse_phase1_2_outcomes('1NNE 2EEN 3TBB', as_list = FALSE)
#' y
#' @references
#' Brock, K., Billingham, L., Copland, M., Siddique, S., Sirovica, M., & Yap, C.
#' (2017). Implementing the EffTox dose-finding design in the Matchpoint trial.
#' BMC Medical Research Methodology, 17(1), 112.
#' https://doi.org/10.1186/s12874-017-0381-x
#'
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#'
parse_phase1_2_outcomes <- function(outcomes, as_list = TRUE) {
  cohorts <- phase1_2_outcomes_to_cohorts(outcomes)
  doses <- integer(length = 0)
  eff <- integer(length = 0)
  tox <- integer(length = 0)
  cohort_ids <- integer(length = 0)
  cohort_id <- 1
  for(cohort in cohorts) {
    c_dl <- cohort$dose
    c_outcomes <- cohort$outcomes

    these_outcomes <- stringr::str_split(c_outcomes, '')[[1]]
    these_eff = as.integer((these_outcomes == 'E') | (these_outcomes == 'B'))
    these_tox = as.integer((these_outcomes == 'T') | (these_outcomes == 'B'))
    these_doses <- rep(c_dl, length(these_tox))

    doses <- c(doses, these_doses)
    eff = c(eff, these_eff)
    tox = c(tox, these_tox)
    cohort_ids <- c(cohort_ids, rep(cohort_id, length(these_doses)))
    cohort_id <- cohort_id + 1
  }

  if(as_list) {
    return(list(
      cohort = cohort_ids, patient = seq_along(tox),
      dose = doses, eff = eff, tox = tox, num_patients = length(doses)
    ))
  } else {
    dose = NULL
    return(tibble(
      cohort = as.integer(cohort_ids),
      patient = seq_along(tox),
      dose = doses,
      tox = tox,
      eff = eff)
    )
  }
}
