
#' Fit the 3+3 model to some outcomes.
#'
#' @param outcomes Outcomes observed. See \code{\link{parse_phase1_outcomes}}.
#' @param num_doses Number of doses under investigation.
#' @param allow_deescalate TRUE to allow de-escalation, as described by Korn et
#' al. Default is FALSE.
#' @param strict_mode TRUE to raise errors if it is detected that the 3+3
#' algorithm has not been followed.
#'
#' @return lits containing recommended_dose and a logical value continue saying
#' whether the trial should continue.
#' @export
#'
#' @examples
#'
#' three_plus_three('2NNN 3NNT', num_doses = 7)
#'
#' @references
#' Storer BE. Design and Analysis of Phase I Clinical Trials. Biometrics.
#' 1989;45(3):925-937. doi:10.2307/2531693
three_plus_three <- function(outcomes, num_doses, allow_deescalate = FALSE,
                             strict_mode = TRUE) {

  # TODO - plumb in allow_deescalate told by Korn et al.

  if(strict_mode) enforce_three_plus_three(outcomes)

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  df_c <- model_frame_to_counts(df, num_doses = num_doses)
  last_dose <- df$dose %>% tail(1)
  if(length(last_dose) == 0) last_dose <- 1
  n_d <- df_c$n[last_dose]
  tox_d <- df_c$tox[last_dose]

  # What to do next is determined by rules that get surprisingly complex:
  if(n_d < 3) {
    # Carry on at this dose
    next_dose <- last_dose
    cont <- TRUE
  } else if(n_d == 3) {
    # We have a decision to make
    if(tox_d == 0) {
      # Escalate if you can, or stop if at top dose
      if(last_dose == num_doses) {
        # Stop and recommended top dose
        next_dose = last_dose
        cont = FALSE
      } else {
        # Escalate and continue
        next_dose = last_dose + 1
        cont = TRUE
      }
    } else if(tox_d == 1) {
      # Remain and continue
      next_dose = last_dose
      cont = TRUE
    } else {
      # More than one tox. De-escalate if allowed, else stop and recommend
      # previous dose or no dose.
      if(allow_deescalate) {
        # Try to de-escalate
        # TODO
        stop('Not implemented yet')
      } else {
        # Stop. Recommend previous dose if possible, or no dose if not.
        if(last_dose == 1) {
          # Recommend no dose and stop
          next_dose <- NA
          cont <- FALSE
        } else {
          # Recommend previous dose and stop
          next_dose <- last_dose - 1
          cont <- FALSE
        }
      }
    }
  } else if(n_d < 6) {
    # Carry on at this dose
    next_dose <- last_dose
    cont <- TRUE
  } else if(n_d == 6) {
    # We have a decision to make
    if(tox_d <= 1) {
      # Escalate if you can, or stop if at top dose
      if(last_dose == num_doses) {
        # Stop and recommended top dose
        next_dose = last_dose
        cont = FALSE
      } else {
        # Escalate and continue
        next_dose = last_dose + 1
        cont = TRUE
      }
    } else {
      # More than one tox. De-escalate if allowed, else stop and recommend
      # previous dose or no dose.
      if(allow_deescalate) {
        # Try to de-escalate
        # TODO
        stop('Not implemented yet')
      } else {
        # Stop. Recommend previous dose if possible, or no dose if not.
        if(last_dose == 1) {
          # Recommend no dose and stop
          next_dose <- NA
          cont <- FALSE
        } else {
          # Recommend previous dose and stop
          next_dose <- last_dose - 1
          cont <- FALSE
        }
      }
    }
  } else {
    warning('Inconsistent 3+3: more than six patients at a dose-level.')
  }

  l <- list(recommended_dose = as.integer(next_dose), continue = cont)
  class(l) <- 'three_plus_three_fit'
  l
}
