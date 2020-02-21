
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
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#'
#' three_plus_three('2NNN 3NNT', num_doses = 7)
#'
#' @references
#' Storer BE. Design and Analysis of Phase I Clinical Trials. Biometrics.
#' 1989;45(3):925-937. doi:10.2307/2531693
#'
#' Korn EL, Midthune D, Chen TT, Rubinstein LV, Christian MC, Simon RM.
#' A comparison of two phase I trial designs. Statistics in Medicine.
#' 1994;13(18):1799-1806. doi:10.1002/sim.4780131802
three_plus_three <- function(outcomes, num_doses, allow_deescalate = FALSE,
                             strict_mode = TRUE) {

  if(strict_mode) enforce_three_plus_three(outcomes,
                                           allow_deescalate = allow_deescalate)

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
      } else if(last_dose < num_doses &
                (df_c$n[last_dose + 1] >= 6 | df_c$tox[last_dose + 1] >= 2)) {
        # Stop and recommended current dose
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
        if(last_dose > 1) {
          # There is room to de-escalate
          if(df_c$n[last_dose - 1] < 6) {
            # Continue at lower dose
            next_dose <- last_dose - 1
            cont <- TRUE
          } else {
            # In Korn et al's words, "The MTD is then defined as the highest dose level
            # ( >= 1) in which 6 patients have been treated with <= 1 instance of DLT"
            n <- NULL
            df_c %>%
              dplyr::filter(n >= 6 & tox <= 1) -> korn_criteria
            if(nrow(korn_criteria) > 0) {
              next_dose <- tail(korn_criteria, 1)$dose
              cont <- FALSE
            } else {
              # No dose is acceptable
              next_dose <- NA
              cont <- FALSE
            }
          }
        } else {
          # There is no room to de-escalate and this dose is too toxic.
          # Stop and recommend no dose
          next_dose = NA
          cont = FALSE
        }
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
      # Escalate if you can. Stop if at top dose or next higher dose has seen 6
      # or next higher dose has seen at least 2 tox
      if(last_dose == num_doses) {
        # Stop and recommended top dose
        next_dose = last_dose
        cont = FALSE
      } else if(last_dose < num_doses &
                (df_c$n[last_dose + 1] >= 6 | df_c$tox[last_dose + 1] >= 2)) {
        # Stop and recommended current dose
        next_dose <- last_dose
        cont <- FALSE
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
        if(last_dose > 1) {
          # There is room to de-escalate
          if(df_c$n[last_dose - 1] < 6) {
            # Continue at lower dose
            next_dose <- last_dose - 1
            cont <- TRUE
          } else {
            # In Korn et al's words, "The MTD is then defined as the highest
            # dose level ( >= 1) in which 6 patients have been treated with <= 1
            # instance of DLT"
            n <- NULL
            df_c %>%
              filter(n >= 6 & tox <= 1) -> korn_criteria
            if(nrow(korn_criteria) > 0) {
              next_dose <- tail(korn_criteria, 1)$dose
              cont <- FALSE
            } else {
              # No dose is acceptable
              next_dose <- NA
              cont <- FALSE
            }
          }
        } else {
          # There is no room to de-escalate and this dose is too toxic.
          # Stop and recommend no dose
          next_dose = NA
          cont = FALSE
        }
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
