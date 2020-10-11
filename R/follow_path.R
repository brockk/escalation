
#' Follow a pre-determined dose administration path.
#'
#' @description
#' This method creates a dose selector that will follow a pre-specified trial
#' path. Whilst the trial path is matched by realised outcomes, the selector
#' will recommend the next dose in the desired sequence. As soon as the observed
#' outcomes diverge from the desired path, the selector stops giving dose
#' recommendations. This makes it possible, for instance, to specify a fixed
#' escalation plan that should be followed until the first toxicity is seen.
#' This tactic is used by some model-based designs to get rapidly to the doses
#' where the action is. See, for example, the dfcrm package and Cheung (2011).
#'
#' @param path Follow this outcome path. See \code{\link{parse_phase1_outcomes}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit a
#' dose-finding model to outcomes.
#'
#' @export
#'
#' @examples
#' model1 <- follow_path(path = '1NNN 2NNN 3NNN 4NNN')
#'
#' fit1 <- model1 %>% fit('1NNN 2N')
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#' # The model recommends continuing at dose 2 because the observed outcomes
#' # perfectly match the desired escalation path.
#'
#' fit2 <- model1 %>% fit('1NNN 2NT')
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#' # Uh oh. Toxicity has now been seen, the outcomes diverge from the sought
#' # path, hence this class recommends no dose now.
#' # At this point, we can hand over dose selection decisions to another class
#' # by chaining them together, like:
#' model2 <- follow_path(path = '1NNN 2NNN 3NNN 4NNN') %>%
#'   get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25)
#' fit3 <- model2 %>% fit('1NNN 2NT')
#' # Now the CRM model is using all of the outcomes to calculate the next dose:
#' fit3 %>% recommended_dose()
#' fit3 %>% continue()
#' @references
#' Cheung. Dose Finding by the Continual Reassessment Method. 2011.
#' Chapman and Hall/CRC. ISBN 9781420091519
follow_path <- function(path) {
  x <- list(path = path)
  class(x) <- c('follow_path_selector_factory', 'selector_factory')
  return(x)
}

#' @importFrom utils head
follow_path_selector <- function(outcomes, path) {

  if(is.character(path)) {
    path_df <- parse_phase1_outcomes(path, as_list = FALSE)
  } else if(is.data.frame(path)) {
    path_df <- spruce_outcomes_df(path)
  } else {
    stop('path should be a character string or a data-frame.')
  }
  if(nrow(path_df) > 0) {
    num_doses <- max(path_df$dose)
  } else {
    num_doses <- 0
  }

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  if(nrow(df) > 0) {
    num_doses <- max(max(df$dose), num_doses)
  }

  df_c <- model_frame_to_counts(df, num_doses = num_doses)
  num_pats <- nrow(df)
  length_path <- nrow(path_df)
  if(length_path == 0) {
    rec_d <- NA
  } else if(num_pats == 0) {
    rec_d <- head(path_df$dose, 1)
  } else if(num_pats >= length_path) {
    rec_d <- NA
  } else if(all(df$dose == path_df$dose[1:num_pats]) &
            all(df$tox == path_df$tox[1:num_pats])) {
    rec_d <- path_df$dose[num_pats + 1]
  } else {
    rec_d <- NA
  }

  l <- list(
    df = df,
    df_c = df_c,
    recommended_dose = rec_d
  )
  class(l) = c( 'follow_path_selector', 'selector')
  l
}

# Factory interface

#' @export
fit.follow_path_selector_factory <- function(selector_factory, outcomes, ...) {
  return(follow_path_selector(outcomes = outcomes,
                              path = selector_factory$path))
}

# Selector interface

#' @export
num_patients.follow_path_selector <- function(x, ...) {
  return(length(x$df$dose))
}

#' @export
cohort.follow_path_selector <- function(x, ...) {
  return(x$df$cohort)
}

#' @export
doses_given.follow_path_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.follow_path_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
num_doses.follow_path_selector <- function(x, ...) {
  return(nrow(x$df_c))
}

#' @export
recommended_dose.follow_path_selector <- function(x, ...) {
  return(x$recommended_dose)
}

#' @export
continue.follow_path_selector <- function(x, ...) {
  return(!is.na(x$recommended_dose))
}

#' @export
tox_at_dose.follow_path_selector <- function(x, ...) {
  return(x$df_c$tox)
}

#' @export
mean_prob_tox.follow_path_selector <- function(x, ...) {
  return(rep(NA, num_doses(x)))
}

#' @export
median_prob_tox.follow_path_selector <- function(x, ...) {
  return(rep(NA, num_doses(x)))
}

#' @export
prob_tox_quantile.follow_path_selector <- function(x, p, ...) {
  return(rep(NA, num_doses(x)))
}

#' @export
prob_tox_exceeds.follow_path_selector <- function(x, threshold, ...) {
  return(rep(NA, num_doses(x)))
}

#' @export
supports_sampling.follow_path_selector <- function(x, ...) {
  return(FALSE)
}

#' @export
prob_tox_samples.follow_path_selector <- function(x, tall = FALSE, ...) {
  stop('follow_path_selector does not support sampling.')
}
