
#' Enforce that a trial path has followed the 3+3 method.
#'
#' This function stops with en error if it detects that outcomes describing  a
#' trial path have diverged from that advocated by the 3+3 method.
#'
#' @param outcomes Outcomes observed. See \code{\link{parse_phase1_outcomes}}.
#'
#' @return Nothing. Function stops if problem detected.
#' @export
#'
#' @examples
#' \dontrun{
#' enforce_three_plus_three('1NNN 2NTN 2NNN')  # OK
#' enforce_three_plus_three('1NNN 2NTN 2N')  # OK too, albeit in-progress cohort
#' enforce_three_plus_three('1NNN 1N')  # Not OK because should have escalated
#' }
enforce_three_plus_three <- function(outcomes) {

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  if(nrow(df) == 0) return()
  df_c <- model_frame_to_counts(df, num_doses = max(df$dose))
  last_dose <- df$dose %>% tail(1)

  if(any(df_c$n > 6)) {
    stop('Inconsistent 3+3 - doses have been given to more than 6 patients.')
  }

  if(nrow(df_c[df_c$n > 3 & df_c$tox == 0, ])) {
    stop('Inconsistent 3+3 - toxless doses given to more than 3 patients.')
  }

  given_doses <- df_c[df_c$n > 0, 'dose', drop = TRUE]
  if(length(given_doses) > 0) {
    if( max(given_doses) - min(given_doses) + 1 != length(given_doses)) {
      stop('Inconsistent 3+3 - some doses have been skipped.')
    }
  }

  mid_cohort <- (df_c$n %% 3) != 0
  if(sum(mid_cohort) > 1) {
    stop('Inconsistent 3+3 - there are several cohorts in progress.')
  }

  if(nrow(df_c) > 1) {
    if(any(df_c[df_c$dose != last_dose, 'n'] %% 3 != 0)) {
      stop('Inconsistent 3+3 - some intermediate cohorts are not complete.')
    }
  }
}
