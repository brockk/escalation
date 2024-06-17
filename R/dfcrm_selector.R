
#' Get an object to fit the CRM model using the dfcrm package.
#'
#' @description
#' This function returns an object that can be used to fit a CRM model using
#' methods provided by the dfcrm package.
#'
#' Dose selectors are designed to be daisy-chained together to achieve different
#' behaviours. This class is a **resumptive** selector, meaning it carries on
#' when the previous dose selector, where present, has elected not to continue.
#' For example, this allows instances of this class to be preceded by a selector
#' that follows a fixed path in an initial escalation plan, such as that
#' provided by \code{\link{follow_path}}. In this example, when the observed
#' trial outcomes deviate from that initial plan, the selector following the
#' fixed path elects not to continue and responsibility passes to this class.
#' See Examples.
#'
#' The time-to-event variant, TITE-CRM, is used via the
#' \code{dfcrm::titecrm} function when you specify \code{tite = TRUE}. This
#' weights the observations to allow dose-selections based on partially observed
#' outcomes.
#'
#' @param parent_selector_factory optional object of type
#' \code{\link{selector_factory}} that is in charge of dose selection before
#' this class gets involved. Leave as NULL to just use CRM from the start.
#' @param skeleton Dose-toxicity skeleton, a non-decreasing vector of
#' probabilities.
#' @param target We seek a dose with this probability of toxicity.
#' @param tite FALSE to use regular CRM; TRUE to use TITE-CRM. See Description.
#' @param ... Extra args are passed to \code{\link[dfcrm]{crm}}.
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' CRM model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm(skeleton = skeleton, target = target)
#'
#' # By default, dfcrm fits the empiric model:
#' outcomes <- '1NNN 2NTN'
#' model1 %>% fit(outcomes) %>% recommended_dose()
#'
#' # But we can provide extra args to get_dfcrm that are than passed onwards to
#' # the call to dfcrm::crm to override the defaults. For example, if we want
#' # the one-parameter logistic model:
#' model2 <- get_dfcrm(skeleton = skeleton, target = target, model = 'logistic')
#' model2 %>% fit(outcomes) %>% recommended_dose()
#' # dfcrm does not offer a two-parameter logistic model but other classes do.
#'
#' # We can use an initial dose-escalation plan, a pre-specified path that
#' # should be followed until trial outcomes deviate, at which point the CRM
#' # model takes over. For instance, if we want to use two patients at each of
#' # the first three doses in the absence of toxicity, irrespective the model's
#' # advice, we would run:
#' model1 <- follow_path('1NN 2NN 3NN') %>%
#'   get_dfcrm(skeleton = skeleton, target = target)
#'
#' # If outcomes match the desired path, the path is followed further:
#' model1 %>% fit('1NN 2N') %>% recommended_dose()
#'
#' # But when the outcomes diverge:
#' model1 %>% fit('1NN 2T') %>% recommended_dose()
#'
#' # Or the pre-specified path comes to an end:
#' model1 %>% fit('1NN 2NN 3NN') %>% recommended_dose()
#' # The CRM model takes over.
#'
#' @references
#' Cheung, K. 2019. dfcrm: Dose-Finding by the Continual Reassessment Method.
#' R package version 0.2-2.1. https://CRAN.R-project.org/package=dfcrm
#'
#' Cheung, K. 2011. Dose Finding by the Continual Reassessment Method.
#' Chapman and Hall/CRC. ISBN 9781420091519
#'
#' O’Quigley J, Pepe M, Fisher L. Continual reassessment method: a practical
#' design for phase 1 clinical trials in cancer. Biometrics. 1990;46(1):33-48.
#' doi:10.2307/2531628
get_dfcrm <- function(parent_selector_factory = NULL, skeleton, target,
                      tite = FALSE, ...) {

  x <- list(
    parent_selector_factory = parent_selector_factory,
    skeleton = skeleton,
    target = target,
    tite = tite,
    extra_args = list(...)
  )

  class(x) <- c('dfcrm_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

#' Get an object to fit the TITE-CRM model using the dfcrm package.
#'
#' @details
#' This function is a short-cut to \code{get_dfcrm(tite = TRUE)}. See
#' \code{\link{get_dfcrm}} for full details.
#'
#' @inheritParams get_dfcrm
#'
#' @return an object of type \code{\link{selector_factory}} that can fit the
#' CRM model to outcomes.
#'
#' @export
#'
#' @examples
#' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' target <- 0.25
#' model1 <- get_dfcrm_tite(skeleton = skeleton, target = target)
#' outcomes <- data.frame(
#'   dose = c(1, 1, 2, 2, 3, 3),
#'   tox = c(0, 0, 0, 0, 1, 0),
#'   weight = c(1, 1, 1, 0.9, 1, 0.5),
#'   cohort = c(1, 2, 3, 4, 5, 6)
#' )
#' fit <- model1 %>% fit(outcomes)
#'
#' @references
#' Cheung, K. 2019. dfcrm: Dose-Finding by the Continual Reassessment Method.
#' R package version 0.2-2.1. https://CRAN.R-project.org/package=dfcrm
#'
#' Cheung, K. 2011. Dose Finding by the Continual Reassessment Method.
#' Chapman and Hall/CRC. ISBN 9781420091519
get_dfcrm_tite <- function(parent_selector_factory = NULL, skeleton, target,
                           ...) {
  get_dfcrm(
    parent_selector_factory = parent_selector_factory,
    skeleton = skeleton,
    target = target,
    tite = TRUE,
    ...
  )
}

#' @importFrom dfcrm crm titecrm
dfcrm_selector <- function(parent_selector = NULL, outcomes, skeleton, target,
                           tite = FALSE, ...) {

  if(is.character(outcomes)) {
    if(tite) {
      stop(paste0("Provide outcomes in a data-frame for TITE-CRM with columns ",
                  "dose, tox and weight"))
    }
    df <- parse_phase1_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  x <- list(
    level = integer(length = 0),
    tox = integer(length = 0),
    mtd = 1,
    ptox = skeleton
  )
  if(nrow(df) > 0) {
    # Checks
    if(max(df$dose) > length(skeleton)) {
      stop('dfcrm_selector - maximum dose given exceeds number of doses.')
    }
    if(tite) {
      if(sum(df$weight) > 0) {
        x <- titecrm(
          prior = skeleton,
          target = target,
          tox = df$tox %>% as.integer(),
          level = df$dose,
          weights = df$weight,
          var.est = TRUE,
          ...
        )
      }
    } else {
      x <- crm(
        prior = skeleton,
        target = target,
        tox = df$tox %>% as.integer(),
        level = df$dose,
        var.est = TRUE,
        ...
      )
    }
  }

  l <- list(
    parent = parent_selector,
    cohort = df$cohort,
    outcomes = outcomes,
    df = df,
    skeleton = skeleton,
    target = target,
    tite = tite,
    dfcrm_fit = x
  )

  class(l) = c('dfcrm_selector', 'tox_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.dfcrm_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    skeleton = selector_factory$skeleton,
    target = selector_factory$target,
    tite = selector_factory$tite
  )
  args <- append(args, selector_factory$extra_args)
  do.call(dfcrm_selector, args = args)
}

#' @export
simulation_function.dfcrm_selector_factory <- function(selector_factory) {
  if(selector_factory$tite) {
    return(phase1_tite_sim)
  } else {
    return(phase1_sim)
  }
}


# Selector interface

#' @export
tox_target.dfcrm_selector <- function(x, ...) {
  return(x$target)
}

#' @export
num_patients.dfcrm_selector <- function(x, ...) {
  return(nrow(x$df))
}

#' @export
cohort.dfcrm_selector <- function(x, ...) {
  return(x$cohort)
}

#' @export
doses_given.dfcrm_selector <- function(x, ...) {
  return(x$df$dose)
}

#' @export
tox.dfcrm_selector <- function(x, ...) {
  return(x$df$tox)
}

#' @export
weight.dfcrm_selector <- function(x, ...) {
  if(x$tite) {
    return(x$df$weight)
  } else {
    return(rep(1, num_patients(x)))
  }
}

#' @export
num_doses.dfcrm_selector <- function(x, ...) {
  return(length(x$skeleton))
}

#' @export
recommended_dose.dfcrm_selector <- function(x, ...) {
  if(!is.null(x$parent)) {
    parent_dose <- recommended_dose(x$parent)
    parent_cont <- continue(x$parent)
    if(parent_cont & !is.na(parent_dose)) {
      return(parent_dose)
    }
  }

  # By default:
  return(as.integer(x$dfcrm_fit$mtd))
}

#' @export
continue.dfcrm_selector <- function(x, ...) {
  # dfcrm offers no methods for stopping but those are provided by other
  # classes in this package.
  # In the daisychain of selectors, this class is resumptive, meaning it will
  # continue with dose-selection after its optional parent, where present, has
  # opted to not continue.
  # Thus, this class always opts to continue:
  return(TRUE)
}

#' @export
mean_prob_tox.dfcrm_selector <- function(x, ...) {
  return(x$dfcrm_fit$ptox)
}

#' @export
median_prob_tox.dfcrm_selector <- function(x, ...) {
  return(prob_tox_quantile(x, p = 0.5))
}

#' @export
#' @importFrom gtools inv.logit
#' @importFrom stats qnorm
prob_tox_quantile.dfcrm_selector <- function(x, p, ...) {
  if(num_patients(x) <= 0) {
    return(as.numeric(rep(NA, num_doses(x))))
  } else {
    beta_hat <- x$dfcrm_fit$estimate
    beta_var <- x$dfcrm_fit$post.var
    # High values for beta lead to low values of prob_tox, so flip the tails:
    beta_q <- qnorm(p = 1 - p, mean = beta_hat, sd = sqrt(beta_var))
    if(x$dfcrm_fit$model == 'empiric') {
      return(x$skeleton ^ exp(beta_q))
    } else if(x$dfcrm_fit$model == 'logistic') {
      alpha <- x$dfcrm_fit$intcpt
      dose_scaled <- x$dfcrm_fit$dosescaled
      inv.logit(alpha + exp(beta_hat) * dose_scaled)
    } else {
      stop(paste0("Don't know what to do with dfcrm model '",
                  x$dfcrm_fit$model, "'"))
    }
  }
}

#' @export
#' @importFrom gtools logit
#' @importFrom stats pnorm
prob_tox_exceeds.dfcrm_selector <- function(x, threshold, ...) {

  if(num_patients(x) <= 0) {
    return(as.numeric(rep(NA, num_doses(x))))
  } else {
    beta_hat <- x$dfcrm_fit$estimate
    beta_var <- x$dfcrm_fit$post.var
    if(x$dfcrm_fit$model == 'empiric') {
      return(pnorm(q = log(log(threshold) / log(x$skeleton)),
                   mean = beta_hat, sd = sqrt(beta_var)))
    } else if(x$dfcrm_fit$model == 'logistic') {
      alpha <- x$dfcrm_fit$intcpt
      dose_scaled <- x$dfcrm_fit$dosescaled
      pnorm(log((logit(threshold) - alpha) / dose_scaled), mean = beta_hat,
            sd = sqrt(beta_var))

    } else {
      stop(paste0("Don't know what to do with dfcrm model '",
                  x$dfcrm_fit$model, "'"))
    }
  }
}

#' @export
supports_sampling.dfcrm_selector <- function(x, ...) {
  return(TRUE)
}

#' @export
#' @importFrom tidyr gather
prob_tox_samples.dfcrm_selector <- function(x, tall = FALSE,
                                            num_samples = 4000,...) {
  df <- get_posterior_prob_tox_sample(x, iter = num_samples)
  if(tall) {
    dose <- prob_tox <- .draw <- NULL
    df %>%
      gather(dose, prob_tox, -.draw)
  } else {
    return(df)
  }
}

#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select everything all_of
summary.dfcrm_selector <- function(object, ...) {
  Dose <- N <- Tox <- EmpiricToxRate <- Skeleton <- NULL
  summary.selector(object) %>%
    mutate(Skeleton = c(NA, object$skeleton)) %>%
    select(
      all_of(Dose),
      all_of(N),
      all_of(Tox),
      all_of(EmpiricToxRate),
      all_of(Skeleton),
      everything()
    )
}
