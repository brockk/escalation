
#' Get an object to fit Wages & Tait's model for phase I/II dose-finding.
#'
#' @description
#' This function returns an object that can be used to fit Wages & Taits model
#' for phase I/II dose-finding, i.e. it selects doses according to efficacy and
#' toxicity outcomes. This function delegates prior-to-posterior calculations to
#' the dfcrm package.
#'
#' @param parent_selector_factory optional object of type
#' \code{\link{selector_factory}} that is in charge of dose selection before
#' this class gets involved. Leave NULL to just use this model from the start.
#' @param tox_skeleton Dose-toxicity skeleton, a non-decreasing vector of
#' probabilities.
#' @param eff_skeletons Matrix of dose-efficacy skeletons, with the skeletons in
#' rows. I.e. number of cols is equal to number of doses, and number of rows is
#' equal to number of efficacy skeletons under consideration.
#' @param eff_skeleton_weights numerical vector, prior weights to efficacy
#' skeletons. Should have length equal to number of rows in
#' \code{eff_skeletons}. Default is equal weights.
#' @param tox_limit We seek a dose with probability of toxicity no greater than
#' this. Value determines the admissible set. See Wages & Tait (2015).
#' @param eff_limit We seek a dose with probability of efficacy no less than
#' this.
#' @param num_randomise integer, maximum number of patients to use in the
#' adaptive randomisation phase of the trial.
#' @param ... Extra args are passed onwards.
#'
#' @return an object of type \code{\link{selector_factory}}.
#'
#' @export
#'
#' @examples
#' # Example in Wages & Tait (2015)
#' tox_skeleton = c(0.01, 0.08, 0.15, 0.22, 0.29, 0.36)
#' eff_skeletons = matrix(nrow=11, ncol=6)
#' eff_skeletons[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20, 0.10)
#' eff_skeletons[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30, 0.20)
#' eff_skeletons[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40, 0.30)
#' eff_skeletons[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50, 0.40)
#' eff_skeletons[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.50)
#' eff_skeletons[6,] <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60)
#' eff_skeletons[7,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.60)
#' eff_skeletons[8,] <- c(0.30, 0.40, 0.50, 0.60, 0.60, 0.60)
#' eff_skeletons[9,] <- c(0.40, 0.50, 0.60, 0.60, 0.60, 0.60)
#' eff_skeletons[10,] <- c(0.50, 0.60, 0.60, 0.60, 0.60, 0.60)
#' eff_skeletons[11,] <- c(rep(0.60, 6))
#' eff_skeleton_weights = rep(1, nrow(eff_skeletons))
#' tox_limit = 0.33
#' eff_limit = 0.05
#' model <- get_wages_and_tait(tox_skeleton = tox_skeleton,
#'                             eff_skeletons = eff_skeletons,
#'                             tox_limit = tox_limit, eff_limit = eff_limit,
#'                             num_randomise = 20)
#' fit <- model %>% fit('1NN 2EN 3BE')
#' fit %>% recommended_dose()
#' fit %>% is_randomising()
#' fit %>% dose_admissible()
#' fit %>% prob_administer()
#'
#' @references
#' Wages, N. A., & Tait, C. (2015).
#' Seamless Phase I/II Adaptive Design for Oncology Trials of Molecularly
#' Targeted Agents.
#' Journal of Biopharmaceutical Statistics, 25(5), 903–920.
#' https://doi.org/10.1080/10543406.2014.920873
get_wages_and_tait <- function(
  parent_selector_factory = NULL, tox_skeleton, eff_skeletons,
  eff_skeleton_weights = rep(1, nrow(eff_skeletons)),
  tox_limit, eff_limit, num_randomise, ...) {

  x <- list(
    parent_selector_factory = parent_selector_factory,
    tox_skeleton = tox_skeleton,
    eff_skeletons = eff_skeletons,
    eff_skeleton_weights = eff_skeleton_weights,
    tox_limit = tox_limit,
    eff_limit = eff_limit,
    num_randomise = num_randomise,
    extra_args = list(...)
  )

  class(x) <- c('wages_tait_selector_factory',
                'eff_tox_selector_factory',
                'selector_factory')
  return(x)
}

#' @importFrom dfcrm crm
wages_tait_selector <- function(
  parent_selector = NULL, outcomes,
  tox_skeleton, eff_skeletons, eff_skeleton_weights,
  tox_limit, eff_limit, num_randomise, ...) {

  if(is.character(outcomes)) {
    df <- parse_phase1_2_outcomes(outcomes, as_list = FALSE)
  } else if(is.data.frame(outcomes)) {
    df <- spruce_outcomes_df(outcomes)
  } else {
    stop('outcomes should be a character string or a data-frame.')
  }

  if(nrow(eff_skeletons) <= 0) {
    stop(paste0('Number of rows in eff_skeletons must be at least 1.'))
  }
  if(ncol(eff_skeletons) != length(tox_skeleton)) {
    stop(paste0('Number of columns in eff_skeletons must equal length of ',
                'tox_skeleton.'))
  }
  if(nrow(eff_skeletons) != length(eff_skeleton_weights)) {
    stop(paste0('Number of rows in eff_skeletons must equal length of ',
                'eff_skeleton_weights'))
  }

  if(nrow(df) > 0) {
    # Checks
    if(max(df$dose) > length(tox_skeleton)) {
      stop('wages_tait_selector - maximum dose given exceeds number of doses.')
    }

    # # Empiric CRM fit for tox model
    # tox_fit <- dfcrm_selector(outcomes = df,
    #                           skeleton = tox_skeleton,
    #                           target = tox_limit,
    #                           model = 'empiric')
    #
    # # Efficacy models also use empiric CRMs:
    # df_c <- model_frame_to_counts(df, num_doses = length(tox_skeleton))
    # # Calculate posterior model weights:
    # eff_weights <- get_empiric_crm_skeleton_weights(
    #   skeletons = eff_skeletons,
    #   events_at_dose = df_c$eff,
    #   n_at_dose = df_c$n,
    #   prior = eff_skeleton_weights)
    # # and fit each model:
    # eff_fits <- lapply(1:nrow(eff_skeletons),
    #                    function(i) dfcrm_selector(
    #                      # Relabel eff as tox to invoke empiric CRM on eff:
    #                      outcomes = df %>% select(-tox) %>% rename(tox = eff),
    #                      skeleton = eff_skeletons[i, ],
    #                      target = eff_limit,
    #                      model = 'empiric'))

  }
  # else {
  #   tox_fit <- list(
  #     level = integer(length = 0),
  #     tox = integer(length = 0),
  #     mtd = 1,
  #     ptox = tox_skeleton)
  #   eff_fits <- lapply(1:nrow(eff_skeletons),
  #                      function(i) list(
  #                        level = integer(length = 0),
  #                        tox = integer(length = 0),
  #                        mtd = NA,
  #                        ptox = eff_skeletons[i, ]
  #                      ))
  #   eff_weights <- eff_skeleton_weights
  # }

  # Empiric CRM fit for tox model
  tox_fit <- dfcrm_selector(outcomes = df,
                            skeleton = tox_skeleton,
                            target = tox_limit,
                            model = 'empiric')

  # Efficacy models also use empiric CRMs:
  df_c <- model_frame_to_counts(df, num_doses = length(tox_skeleton))
  # Calculate posterior model weights:
  eff_weights <- get_empiric_crm_skeleton_weights(
    skeletons = eff_skeletons,
    events_at_dose = df_c$eff,
    n_at_dose = df_c$n,
    prior = eff_skeleton_weights)
  # and fit each model:
  eff_fits <- lapply(1:nrow(eff_skeletons),
                     function(i) dfcrm_selector(
                       # Relabel eff as tox to invoke empiric CRM on eff:
                       outcomes = df %>% select(-tox) %>% rename(tox = eff),
                       skeleton = eff_skeletons[i, ],
                       target = eff_limit,
                       model = 'empiric'))

  l <- list(
    parent = parent_selector,
    tox_skeleton = tox_skeleton,
    tox_limit = tox_limit,
    tox_fit = tox_fit,
    eff_skeletons = eff_skeletons,
    eff_weights = eff_weights,
    eff_limit = eff_limit,
    eff_fits = eff_fits,
    eff_model_index = which.max(eff_weights),
    num_randomise = num_randomise
  )

  class(l) = c('wages_tait_selector', 'eff_tox_selector', 'selector')
  l
}

# Factory interface

#' @importFrom magrittr %>%
#' @export
fit.wages_tait_selector_factory <- function(selector_factory, outcomes, ...) {

  if(is.null(selector_factory$parent)) {
    parent <- NULL
  } else {
    parent <- selector_factory$parent %>% fit(outcomes, ...)
  }

  args <- list(
    parent = parent,
    outcomes = outcomes,
    tox_skeleton = selector_factory$tox_skeleton,
    eff_skeletons = selector_factory$eff_skeletons,
    eff_skeleton_weights = selector_factory$eff_skeleton_weights,
    tox_limit = selector_factory$tox_limit,
    eff_limit = selector_factory$eff_limit,
    num_randomise = selector_factory$num_randomise
  )
  args <- append(args, selector_factory$extra_args)
  do.call(wages_tait_selector, args = args)
}


#' @export
tox_limit.wages_tait_selector <- function(x, ...) {
  return(x$tox_limit)
}

#' @export
eff_limit.wages_tait_selector <- function(x, ...) {
  return(x$eff_limit)
}

#' @export
num_patients.wages_tait_selector <- function(x, ...) {
  return(num_patients(x$tox_fit, ...))
}

#' @export
cohort.wages_tait_selector <- function(x, ...) {
  return(cohort(x$tox_fit, ...))
}

#' @export
doses_given.wages_tait_selector <- function(x, ...) {
  return(doses_given(x$tox_fit, ...))
}

#' @export
tox.wages_tait_selector <- function(x, ...) {
  return(tox(x$tox_fit, ...))
}

#' @export
eff.wages_tait_selector <- function(x, ...) {
  # Arbitrarily use first efficacy sub-model. They use identical outcomes.
  # Recall: the 'tox' event in the efficacy models is actually eff:
  return(tox(x$eff_fit[[1]], ...))
}

#' @export
num_doses.wages_tait_selector <- function(x, ...) {
  return(num_doses(x$tox_fit, ...))
}

#' @export
#' @importFrom binom binom.confint
recommended_dose.wages_tait_selector <- function(x, ...) {
  if(!is.null(x$parent)) {
    parent_dose <- recommended_dose(x$parent)
    parent_cont <- continue(x$parent)
    if(parent_cont & !is.na(parent_dose)) {
      return(parent_dose)
    }
  }

  # Pursue Wages & Tait's method:
  admiss <- dose_admissible(x)
  if(sum(admiss) > 0) {
    # There is at least one admissible dose
    if(num_patients(x) < x$num_randomise) {
      # We are in the adaptive randomisation stage of the trial
      prob_rand <- prob_administer(x)
      d_ <- dose_indices(x)
      return(sample(x = d_, size = 1, replace = FALSE, prob = prob_rand))
    } else {
      # We are in the maximisation stage of the trial
      prob_eff_ <- mean_prob_eff(x) * as.integer(admiss)
      rec_d <- which.max(prob_eff_)
      n_ <- n_at_dose(x)
      if(n_[rec_d] > 0) {
        # The recommended dose has been evaluated before
        e_ <- eff_at_dose(x)
        prob_eff_rec_ci <- binom.confint(x = e_[rec_d], n_[rec_d],
                                         conf.level = 0.95, methods = 'exact')
        if(prob_eff_rec_ci$upper < x$eff_limit) {
          # Best dose is insufficiently efficacious. Recommend no dose:
          return(NA)
        }
      }

      # Otherwise
      return(rec_d)
    }
  } else {
    # There are no admissible doses:
    return(NA)
  }
}

#' @export
#' @importFrom binom binom.confint
continue.wages_tait_selector <- function(x, ...) {
  if(num_patients(x) > 0) {
    admiss <- dose_admissible(x)
    if(sum(admiss) <= 0) {
      return(FALSE)
    }

    n_ <- n_at_dose(x)
    t_ <- tox_at_dose(x)
    if(n_[1] > 0) {
      # Lowest dose has been given
      prob_tox_1_ci <- binom.confint(x = t_[1], n_[1], conf.level = 0.95,
                                     methods = 'exact')
      if(prob_tox_1_ci$lower > x$tox_limit) {
        # Lowest dose is too toxic
        return(FALSE)
      }

      if(num_patients(x) >= x$num_randomise) {
        # We are in the maximisation trial stage
        rec_d <- recommended_dose(x)
        if(is.na(rec_d))
          # No dose is recommended, so stop:
          return(FALSE)

        if(n_[rec_d] > 0) {
          # The recommended dose has been evaluated before
          e_ <- eff_at_dose(x)
          prob_eff_rec_ci <- binom.confint(x = e_[rec_d], n_[rec_d],
                                           conf.level = 0.95, methods = 'exact')
          if(prob_eff_rec_ci$upper < x$eff_limit) {
            # Recommended dose is inefficacious, so stop:
            return(FALSE)
          }
        }
      }
    }
  }

  # In all other circumstances:
  return(TRUE)
}

#' @export
is_randomising.wages_tait_selector <- function(x, ...) {
  return(num_patients(x) < x$num_randomise)
}

#' @export
prob_administer.wages_tait_selector <- function(x, ...) {
  if(num_patients(x) < x$num_randomise) {
    admiss <- dose_admissible(x)
    if(sum(admiss)) {
      prob_rand <- mean_prob_eff(x) * as.integer(admiss)
      prob_rand <- prob_rand / sum(prob_rand)
      return(prob_rand)
    } else {
      return(rep(0, num_doses(x)))
    }
  } else {
    n_ <- n_at_dose(x)
    return(n_ / sum(n_))
  }
}

#' @export
mean_prob_tox.wages_tait_selector <- function(x, ...) {
  return(mean_prob_tox(x$tox_fit, ...))
}

#' @export
median_prob_tox.wages_tait_selector <- function(x, ...) {
  return(median_prob_tox(x$tox_fit, ...))
}

#' @export
mean_prob_eff.wages_tait_selector <- function(x, ...) {
  # Recall: the 'tox' event in the efficacy models is actually eff:
  return(mean_prob_tox(x$eff_fits[[x$eff_model_index]], ...))
}

#' @export
median_prob_eff.wages_tait_selector <- function(x, ...) {
  # Recall: the 'tox' event in the efficacy models is actually eff:
  return(median_prob_tox(x$eff_fits[[x$eff_model_index]], ...))
}

#' @export
dose_admissible.wages_tait_selector <- function(x, ...) {
  if(num_patients(x) > 0)
    return(mean_prob_tox(x$tox_fit, ...) <= x$tox_limit )
  else
    return(rep(TRUE, num_doses(x)))
}

#' @export
prob_tox_quantile.wages_tait_selector <- function(x, p, ...) {
  return(prob_tox_quantile(x$tox_fit, p = p, ...))
}

#' @export
prob_tox_exceeds.wages_tait_selector <- function(x, threshold, ...) {
  return(prob_tox_exceeds(x$tox_fit, threshold = threshold, ...))
}

#' @export
supports_sampling.wages_tait_selector <- function(x, ...) {
  return(supports_sampling(x$tox_fit, ...))
}

#' @export
prob_eff_quantile.wages_tait_selector <- function(x, p, ...) {
  # Recall: the 'tox' event in the efficacy models is actually eff:
  return(prob_tox_quantile(x$eff_fits[[x$eff_model_index]], p = p, ...))
}

#' @export
prob_eff_exceeds.wages_tait_selector <- function(x, threshold, ...) {
  # Recall: the 'tox' event in the efficacy models is actually eff:
  return(prob_tox_exceeds(x$eff_fits[[x$eff_model_index]],
                          threshold = threshold, ...))
}

#' @export
prob_tox_samples.wages_tait_selector <- function(x, tall = FALSE,
                                                 num_samples = 4000,...) {
  return(prob_tox_samples(x$tox_fit, tall = tall, num_samples = num_samples,
                          ...))
}

#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
prob_eff_samples.wages_tait_selector <- function(x, tall = FALSE,
                                                 num_samples = 4000, ...) {
  # Recall: the 'tox' event in the efficacy models is actually eff:
  prob_eff <- prob_tox <- NULL
  if(tall) {
    return(prob_tox_samples(x$eff_fits[[x$eff_model_index]],
                            tall = tall, num_samples = num_samples, ...)) %>%
      rename(prob_eff = prob_tox)
  } else {
    return(prob_tox_samples(x$eff_fits[[x$eff_model_index]],
                            tall = tall, num_samples = num_samples, ...))
  }
}
