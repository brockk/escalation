
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
#' # Uh oh. Toxicity has now been seen. This class recommends no dose now.
#' @references
#' Cheung. Dose Finding by the Continual Reassessment Method. 2011.
#' Chapman and Hall/CRC. ISBN 9781420091519
follow_path <- function(path) {
  x <- list(path = path)
  class(x) <- c('selector_factory', 'follow_path_selector_factory')
  return(x)
}

follow_path_selector <- function(outcomes, path) {

  if(is.character(outcomes)) {
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }
  df_c <- model_frame_to_counts(df)

  if(is.character(path)) {
    path_df <- parse_phase1_outcomes(path, as_list = FALSE)
  } else if(is.data.frame(path)) {
    path_df <- spruce_outcomes_df(path)
  } else {
    stop('path should be a character string or a data-frame.')
  }

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
    df = df, df_c = df_c,
    recommended_dose = rec_d
  )
  class(l) = c('selector', 'follow_path_selector')
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
recommended_dose.follow_path_selector <- function(selector, ...) {
  return(selector$recommended_dose)
}


#' @export
continue.follow_path_selector <- function(selector, ...) {
  return(!is.na(selector$recommended_dose))
}
