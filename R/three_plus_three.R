
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
three_plus_three <- function(outcomes, num_doses, allow_deescalate = FALSE,
                             strict_mode = TRUE) {

  # TODO - plumb in allow_deescalate told by Korn et al.

  df <- parse_phase1_outcomes(outcomes)
  df_c <- phase1_outcomes_to_counts(outcomes = outcomes, num_doses = num_doses)
  last_dose <- df$dose %>% tail(1)
  if(length(last_dose) == 0) last_dose <- 1

  if(strict_mode) {
    # Stop the trial if you detect the algo has not been followed:
    mid_cohort <- (df_c$n %% 3) != 0
    if(sum(mid_cohort) > 1)
      stop('Inconsistent 3+3 - there are several cohorts in progress.')

    if(any(df_c[df_c$dose != last_dose, 'n'] %% 3 != 0))
      stop('Inconsistent 3+3 - some intermediate cohorts are not complete.')

    given_doses <- df_c[df_c$n > 0, 'dose', drop = TRUE]
    if(length(given_doses) > 0) {
      if( max(given_doses) - min(given_doses) + 1 != length(given_doses))
        stop('Inconsistent 3+3 - some doses have been skipped.')
    }

    if(nrow(df_c[df_c$n > 3 & df_c$tox == 0, ]))
      stop('Inconsistent 3+3 - toxless doses given to more than 3 patients.')

    if(any(df_c$n > 6))
      stop('Inconsistent 3+3 - doses have ben given to more than 6 patients.')
  }

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

  l <- list(recommended_dose = next_dose, continue = cont)
  class(l) <- 'three_plus_three_fit'
  l
}
