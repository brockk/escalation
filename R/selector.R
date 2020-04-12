
#' Dose selector.
#'
#' @description
#' This is a core class in this package. It encapsulates that an object (e.g. a
#' CRM model, a 3+3 model) is able to recommend doses, keep track of how many
#' patients have been treated at what doses, what toxicity outcomes have been
#' seen, and whether a trial should continue. It offers a consistent interface
#' to dose-finding methods from several packages, including \code{dfcrm} and
#' \code{BOIN}.  \code{bcrm} and \code{trialr} will be added.
#'
#' Once you have a standardised interface, modularisation offers a powerful way
#' to adorn dose-finding methods with extra desirable behaviour. \code{selector}
#' objects can be daisy-chained togther using \code{magrittr}'s pipe operator.
#' For instance, the CRM fitting method in \code{dfcrm} is fantastic because it
#' runs quickly and is simple to call. However, it does not recommend that a
#' trial stops if a dose is too toxic or if n patients have already been treated
#' at the recommended dose. Each of these behaviours can be bolted on via
#' additional selectors. Furthermore, those behaviours and more can be bolted
#' on to any dose selector because of the modular approach implemented in
#' \code{escalation}. See Examples.
#'
#' \code{selector} objects are obtained by calling the \code{\link{fit}}
#' function on a \code{\link{selector_factory}} object.
#' A \code{\link{selector_factory}} object is obtained by initially calling a
#' function like \code{\link{get_dfcrm}}, \code{\link{get_three_plus_three}} or
#' \code{\link{get_boin}}. Users may then add desired extra behaviour with
#' subsequent calls to functions like \code{\link{stop_when_n_at_dose}} or
#' \code{\link{stop_when_too_toxic}}.
#'
#' The \code{selector} class also supports that an object will be able to
#' perform inferential calculations on the rates of toxicity via functions like
#' \code{\link{mean_prob_tox}}, \code{\link{median_prob_tox}}, and
#' \code{\link{prob_tox_exceeds}}. However, naturally the sophistication of
#' those calculations will vary by model implementation. For example, a full
#' MCMC method will be able to quantify any probability you like by working with
#' posterior samples. In contrast, a method like the \code{\link[dfcrm]{crm}}
#' function in \code{dfcrm} that uses the plug-in method to estimate posterior
#' dose-toxicity curves cannot natively estimate the median probability of tox.
#'
#' @details Every \code{selector} object implements the following functions:
#' \itemize{
#'   \item \code{\link{tox_target}}
#'   \item \code{\link{num_patients}}
#'   \item \code{\link{cohort}}
#'   \item \code{\link{doses_given}}
#'   \item \code{\link{tox}}
#'   \item \code{\link{num_tox}}
#'   \item \code{\link{model_frame}}
#'   \item \code{\link{num_doses}}
#'   \item \code{\link{recommended_dose}}
#'   \item \code{\link{continue}}
#'   \item \code{\link{n_at_dose}}
#'   \item \code{\link{n_at_recommended_dose}}
#'   \item \code{\link{dose_indices}}
#'   \item \code{\link{prob_administer}}
#'   \item \code{\link{tox_at_dose}}
#'   \item \code{\link{empiric_tox_rate}}
#'   \item \code{\link{mean_prob_tox}}
#'   \item \code{\link{median_prob_tox}}
#'   \item \code{\link{prob_tox_quantile}}
#'   \item \code{\link{prob_tox_exceeds}}
#' }
#'
#' @seealso \code{\link{selector_factory}}
#'
#' @export
#'
#' @examples
#'
#' # Start with a simple CRM model
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#'
#' # Add a rule to stop when 9 patients are treated at the recommended dose
#' model2 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_n_at_dose(n = 9, dose = 'recommended')
#'
#' # Add a rule to stop if toxicity rate at lowest dose likely exceeds target
#' model3 <- get_dfcrm(skeleton = skeleton, target = target) %>%
#'   stop_when_n_at_dose(n = 9, dose = 'recommended') %>%
#'   stop_when_too_toxic(dose = 1, tox_threshold = target, confidence = 0.5)
#'
#' # We now have three CRM models that differ in their stopping behaviour.
#' # Let's fit each to some outcomes to see those differences:
#'
#' outcomes <- '1NNN 2NTT 1NNT'
#' fit1 <- model1 %>% fit(outcomes)
#' fit2 <- model2 %>% fit(outcomes)
#' fit3 <- model3 %>% fit(outcomes)
#'
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' fit3 %>% recommended_dose()
#' fit3 %>% continue()
#' # Already model3 wants to stop because of excessive toxicity.
#'
#' # Let's carry on with models 1 and 2 by adding another cohort:
#'
#' outcomes <- '1NNN 2NTT 1NNT 1NNN'
#' fit1 <- model1 %>% fit(outcomes)
#' fit2 <- model2 %>% fit(outcomes)
#'
#' fit1 %>% recommended_dose()
#' fit1 %>% continue()
#'
#' fit2 %>% recommended_dose()
#' fit2 %>% continue()
#'
#' # Model1 wants to continue - in fact it will never stop.
#' # In contrast, model2 has seen 9 at dose 1 so, rather than suggest dose 1
#' # again, it suggests the trial should stop.
#'
#' # For contrast, let us consider a BOIN model on the same outcomes
#' boin_fitter <- get_boin(num_doses = length(skeleton), target = target)
#' fit4 <- boin_fitter %>% fit(outcomes)
#' fit4 %>% recommended_dose()
#' fit4 %>% continue()
#'
#' # Full selector interface:
#' fit <- fit2
#' fit %>% tox_target()
#' fit %>% num_patients()
#' fit %>% cohort()
#' fit %>% doses_given()
#' fit %>% tox()
#' fit %>% num_tox()
#' fit %>% model_frame()
#' fit %>% num_doses()
#' fit %>% dose_indices()
#' fit %>% recommended_dose()
#' fit %>% continue()
#' fit %>% n_at_dose()
#' fit %>% n_at_recommended_dose()
#' fit %>% prob_administer()
#' fit %>% tox_at_dose()
#' fit %>% empiric_tox_rate()
#' fit %>% mean_prob_tox()
#' fit %>% median_prob_tox()
#' fit %>% prob_tox_quantile(0.9)
#' fit %>% prob_tox_exceeds(0.5)
selector <- function() {
  # This function exists only to document the abstract class "selector".
}

#' @export
tox_target.selector <- function(x, ...) {
  # By default:
  return(NULL)
}

#' @export
num_tox.selector <- function(x, ...) {
  sum(tox(x))
}

#' @export
#' @importFrom tibble tibble
model_frame.selector <- function(x, ...) {

  if(num_patients(x) > 0) {
    tibble(
      patient = seq(1, num_patients(x)),
      cohort = cohort(x) %>% as.integer(),
      dose = doses_given(x) %>% as.integer(),
      tox = tox(x) %>% as.integer()
    )
  } else {
    tibble(
      patient = integer(length = 0),
      cohort = integer(length = 0),
      dose = integer(length = 0),
      tox = integer(length = 0)
    )
  }
}

#' @export
dose_indices.selector <- function(x, ...) {
  n <- num_doses(x)
  if(n > 0) {
    return(1:n)
  } else {
    return(integer(length = 0))
  }
}

#' @importFrom purrr map_int
#' @export
n_at_dose.selector <- function(x, dose = NULL, ...) {
  if(is.null(dose)) {
    dose_indices <- 1:(num_doses(x))
    map_int(dose_indices, ~ sum(doses_given(x) == .x))
  } else if(dose == 'recommended') {
    n_at_recommended_dose(x)
  } else {
    sum(doses_given(x) == dose)
  }
}

#' @export
n_at_recommended_dose.selector <- function(x, ...) {
  rec_d <- recommended_dose(x)
  if(is.na(rec_d)) {
    return(NA)
  }
  else {
    return(n_at_dose(x)[rec_d])
  }
}

#' @export
prob_administer.selector <- function(x, ...) {
  n_doses <- num_doses(x)
  n_d <- n_at_dose(x)
  names(n_d) <- 1:n_doses
  n_d / sum(n_d)
}

#' @export
empiric_tox_rate.selector <- function(x, ...) {
  return(x %>% tox_at_dose() / x %>% n_at_dose())
}

#' @export
summary.selector <- function(object, ...) {
  Dose <- N <- Tox <- EmpiricToxRate <- MeanProbTox <- MedianProbTox <- NULL
  tibble(
    Dose = dose_indices(object),
    N = n_at_dose(object),
    Tox = tox_at_dose(object),
    EmpiricToxRate = empiric_tox_rate(object),
    MeanProbTox = mean_prob_tox(object),
    MedianProbTox = median_prob_tox(object)
  )
}

#' @importFrom stringr str_to_title
#' @importFrom tibble tibble
#' @export
print.selector <- function(x, ...) {

  # Patient-level data
  if(num_patients(x) > 0) {
    cat('Patient-level data:\n')
    df <- model_frame(x)
    colnames(df) <- str_to_title(colnames(df))
    print(df)
  } else {
    cat('No patients have been treated.\n')
  }
  cat('\n')

  # Dose-level data
  if(num_doses(x) > 0) {
    cat('Dose-level data:\n')
    df <- summary(x)
    print(df, digits = 3)
  } else {
    cat('No doses are under investigation.\n')
  }
  cat('\n')

  # Toxicity target
  tt <- tox_target(x)
  if(!is.null(tt)) {
    if(!is.na(tt)) {
      cat(paste0('The model targets a toxicity level of ', tt, '.'))
      cat('\n')
    }
  }

  # Dose recommendation and continuance
  recd <- recommended_dose(x)
  cont <- continue(x)
  if(is.na(recd)) {
    if(cont) {
      cat(paste0('The model advocates continuing but recommends no dose.'))
    } else {
      cat(paste0('The model advocates stopping and recommending no dose.'))
    }
  } else {
    if(cont) {
      cat(paste0('The model advocates continuing at dose ', recd, '.'))
    } else {
      cat(paste0('The model advocates stopping and recommending dose ', recd,
                 '.'))
    }
  }
  cat('\n')

  # cat(paste0('The dose most likely to be the MTD is ',
  #            x$modal_mtd_candidate, '.'))
  # cat('\n')
  # cat(paste0('Model entropy: ', format(round(x$entropy, 2), nsmall = 2)))
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.selector <- function(x, ...) {

  dose_labs <- c('NoDose', as.character(dose_indices(x)))
  rec_d <- recommended_dose(x)
  if(is.na(rec_d)) {
    rec_bool <- c(TRUE, rep(FALSE, num_doses(x)))
  } else {
    rec_bool <- c(FALSE, dose_indices(x) == rec_d)
  }

  tibble(
    dose = ordered(dose_labs, levels = dose_labs),
    tox = c(0, tox_at_dose(x)),
    n = c(0, n_at_dose(x)),
    empiric_tox_rate = c(0, empiric_tox_rate(x)),
    mean_prob_tox = c(0, mean_prob_tox(x)),
    median_prob_tox = c(0, median_prob_tox(x)),
    recommended = rec_bool
  )
}
