
# Cruft ----
# ?
# selector_factory <- crm_fitter
# true_prob_tox = c(0.1, 0.27, 0.38, 0.45, 0.61)
# sample_patient_arrivals = function() cohorts_of_n(n = 3, mean_time_delta = 1)
# previous_outcomes = '1NNN 2NNT'
# next_dose = NULL
# i_like_big_trials = FALSE



# Helpers.R ----
#' @importFrom stats rexp
cohorts_of_n <- function(n = 3, mean_time_delta = 1) {
  time_delta <- rexp(n = n, rate = 1 / mean_time_delta) %>% round(1)
  data.frame(time_delta = time_delta)
}


# New factory interface ----
simulation_function <- function(selector_factory) {
  UseMethod('simulation_function')
}

dose_paths_function <- function(selector_factory) {
  UseMethod('dose_paths_function')
}


# New concrete factory interface ----
simulation_function.derived_dose_selector_factory <- function(selector_factory){
  return(selector_factory$parent %>% simulation_function())
}

simulation_function.tox_selector_factory <- function(selector_factory) {
  return(phase1_sim)
}

dose_paths_function.derived_dose_selector_factory <- function(selector_factory){
  return(selector_factory$parent %>% dose_paths_function())
}

dose_paths_function.tox_selector_factory <- function(selector_factory) {
  return(phase1_dose_paths)
}



# New interface just for simulations ----
prob_recommend <- function(simulations, ...) {
  UseMethod('prob_recommend')
}

trial_duration <- function(simulations, ...) {
  UseMethod('trial_duration')
}


# New concrete interface for selector ----
as_tibble.selector <- function(selector, ...) {
  tibble(
    dose = dose_indices(selector),
    tox = tox_at_dose(selector),
    n = n_at_dose(selector),
    empiric_tox_rate = empiric_tox_rate(selector),
    mean_prob_tox = mean_prob_tox(selector),
    median_prob_tox = median_prob_tox(selector),
    recommended_dose = ifelse(
      is.na(recommended_dose(selector)),
      rep(FALSE, num_doses(selector)),
      recommended_dose(selector) == dose_indices(selector)
    )
  )
}

# New concrete interface for simulations ----
#' @importFrom purrr map map_int
#' @importFrom magrittr %>%
num_patients.simulations <- function(simulations, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map_int(num_patients)
}


#' @importFrom utils head
num_doses.simulations <- function(simulations, ...) {
  # Have a word with this amount of nesting!
  num_doses(head(simulations, 1)[[1]][[1]]$fit)
}

dose_indices.simulations <- function(dose_indices, ...) {
  n_d <- num_doses(dose_indices)
  if(n_d > 0) {
    1:n_d
  } else {
    integer(length = 0)
  }
}

#' @importFrom purrr map map_int
#' @importFrom magrittr %>%
#' @importFrom utils tail
recommended_dose.simulations <- function(simulations, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map_int(recommended_dose)
}

#' @importFrom purrr map imap_int
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
n_at_dose.simulations <- function(simulations, dose = NULL, ...) {

  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map(n_at_dose) %>%
    do.call(what = rbind) -> x
  colnames(x) <- dose_indices(simulations)
  x <- x %>% as_tibble()

  if(is.null(dose)) {
    return(x)
  } else if(dose == 'recommended') {
    rec_d <- recommended_dose(simulations)
    return(imap_int(rec_d, ~ ifelse(is.na(.x), NA, x[.y, .x, drop = TRUE])))
  } else {
    stop(paste0("Don't know what to do with dose = '", dose, "'"))
  }

}

#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom tibble as_tibble
tox_at_dose.simulations <- function(simulations, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map('fit') %>%
    map(tox_at_dose) %>%
    do.call(what = rbind) -> x
  colnames(x) <- dose_indices(simulations)
  x %>% as_tibble()
}

num_tox.simulations <- function(simulations, ...) {
  rowSums(tox_at_dose(simulations, ...))
}

#' @importFrom purrr map_int
prob_recommend.simulations <- function(simulations, ...) {
  if(length(simulations) > 0) {
    n_doses <- num_doses(simulations)
    rec_d <- recommended_dose(simulations)
    x <- c(sum(is.na(rec_d)),
           map_int(1:n_doses, ~ sum(rec_d == .x, na.rm = TRUE)))
    names(x) <- c('NoDose', 1:n_doses)
    x / sum(x)
  } else {
    return(NULL)
  }
}

#' @importFrom utils head tail
#' @importFrom magrittr %>%
#' @importFrom purrr map_int
prob_administer.simulations <- function(simulations, method = 0, ...) {
  if(length(simulations) > 0) {
    if(method == 0) {
      n_doses <- num_doses(simulations)
      total_n_at_dose <- n_at_dose(simulations) %>% colSums()
      names(total_n_at_dose) <- 1:n_doses
      total_n_at_dose / sum(total_n_at_dose)
    } else if(method == 1) {
      simulations %>%
        map(~ tail(.x, 1)[[1]]) %>%
        map(prob_administer) %>%
        do.call(what = rbind)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom purrr map map_chr
trial_duration.simulations <- function(simulations, method = 0, ...) {
  simulations %>%
    map(~ tail(.x, 1)[[1]]) %>%
    map_chr('time') %>%
    as.numeric()
}

#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom purrr imap_dfr map_dfr
#' @importFrom dplyr mutate select everything
as_tibble.simulations <- function(simulations, ...) {
  simulations %>%
    imap_dfr(.f = function(x, i) {
      map_dfr(x, function(y) {
        as_tibble(y$fit) %>%
          mutate(
            .iteration = i,
            # cohort = y$cohort,
            .depth = y$.depth,
            time = y$time
          )
      })
    }) %>%
    select(.iteration, .depth, time, everything())
    # select(.iteration, time, cohort, everything())
}

# simulate.R ----
library(purrr)
simulate <- function(selector_factory, num_sims, ...) {
  sim_func <- selector_factory %>% simulation_function()
  l <- lapply(1:num_sims, function(x) sim_func(selector_factory, ...))
  class(l) <- 'simulations'
  l
}

#' @importFrom stats rbinom
#' @importFrom magrittr %>%
#' @importFrom utils tail
phase1_sim <- function(
  selector_factory,
  true_prob_tox,
  sample_patient_arrivals = function(df) cohorts_of_n(n=3, mean_time_delta=1),
  previous_outcomes = '',
  next_dose = NULL,
  i_like_big_trials = FALSE, # Safety net if stop_trial_func is mis-specified...
  return_all_fits = FALSE
) {
  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(previous_outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  } else{
    base_df <- parse_phase1_outcomes('', as_list = FALSE)
  }
  dose <- base_df$dose
  tox <- base_df$tox
  cohort <- base_df$cohort
  next_cohort <- ifelse(length(cohort) > 0, max(cohort) + 1, 1)
  if('time' %in% colnames(base_df)) {
    time <- previous_outcomes$time
  } else {
    time <- rep(0, length(dose))
  }

  i <- 1 # loop counter
  max_i <- 30
  time_now <- 0
  fit <- selector_factory %>% fit(base_df)
  if(is.null(next_dose)) next_dose <- fit %>% recommended_dose()
  fits <- list()
  fits[[1]] <- list(.depth = i, time = time_now, fit = fit)
  while(fit %>% continue() & !is.na(next_dose) &
        (i_like_big_trials | i < max_i)) {

    current_data = data.frame(
      cohort = cohort,
      patient = seq_along(dose),
      dose = dose,
      tox = tox,
      time = time
    )
    new_pts <- sample_patient_arrivals(current_data)
    arrival_time_deltas <- cumsum(new_pts$time_delta)
    n_new_pts <- nrow(new_pts)
    new_dose <- rep(next_dose, n_new_pts)
    new_tox <- rbinom(n = n_new_pts, size = 1, prob = true_prob_tox[next_dose])
    new_cohort <- rep(next_cohort, n_new_pts)

    dose <- c(dose, new_dose)
    tox <- c(tox, new_tox)
    cohort <- c(cohort, new_cohort)
    time <- c(time, time_now + arrival_time_deltas)
    new_data = data.frame(
      cohort = cohort,
      patient = 1:length(dose),
      dose = dose,
      tox = tox,
      time = time
    )

    time_now <- time_now + max(arrival_time_deltas)
    i <- i + 1
    fit <- selector_factory %>% fit(new_data)
    next_cohort <- next_cohort + 1
    fits[[i]] <- list(.depth = i, time = time_now, fit = fit)
    next_dose <- fit %>% recommended_dose()
  }

  # Warn about i_like_big_trials if sim stopped because of too big i.
  if(!i_like_big_trials & i >= max_i) {
    warning(paste(
      "Simulation stopped because max depth reached.",
      "Set 'i_like_big_trials = TRUE' to avoid this constraint. "))
  }

  if(return_all_fits) {
    return(fits)
  } else {
    return(tail(fits, 1))
  }
}


# get_dose_paths.R ----
get_dose_paths <- function(selector_factory, cohort_sizes, ...) {
  dose_paths_func <- selector_factory %>% dose_paths_function()
  # l <- lapply(1:num_sims, function(x) dose_paths_func(selector_factory, ...))
  # class(l) <- 'dose_paths'
  # l
  dose_paths_func(selector_factory, cohort_sizes = cohort_sizes, ...)
}

dose_finding_path_node <- function(node_id, parent_node_id, depth, outcomes,
                                   next_dose, fit, parent_fit) {
  x <- list(.node = node_id,
            .parent = parent_node_id,
            .depth = depth,
            outcomes = outcomes,
            next_dose = next_dose,
            fit = fit,
            parent_fit = parent_fit)
  class(x) <- c("dose_finding_path_node")
  x
}

#' Cast \code{dose_paths} object to \code{\link[tibble]{tibble}}.
#'
#' @param x Object of class \code{dose_finding_paths}.
#' @param ... Extra args passed onwards.
#'
#' @return Object of class \code{\link[tibble]{tibble}}
#'
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @export
as_tibble.dose_paths <- function(x, ...) {
  fit <- NULL
  tibble(
    .node = map_dbl(x, '.node'),
    .parent = map_dbl(x, '.parent'),
    .depth = map_dbl(x, '.depth'),
    outcomes = map_chr(x, 'outcomes'),
    next_dose = map_dbl(x, 'next_dose'),
    fit = map(x, 'fit'),
    parent_fit = map(x, 'parent_fit'),
    dose_index = map(fit, 'dose_indices'),
    ...
  )
}

library(gtools)

#' @importFrom gtools combinations
phase1_dose_paths <- function(
  selector_factory,
  cohort_sizes,
  previous_outcomes = '',
  next_dose = NULL,
  verbose = FALSE,
# i_am_patient = FALSE,
...) {

  if(!all(cohort_sizes == ceiling(cohort_sizes))) {
    stop('cohort_sizes must be strictly positive integers.')
  }
  if(!all(cohort_sizes > 0)) {
    stop('cohort_sizes must be strictly positive integers.')
  }

  max_depth <- length(cohort_sizes)
  num_paths = 1 + sum(sapply(1:max_depth,
                             function(i) prod((cohort_sizes + 1)[1:i])))
  if(num_paths >= 100 & num_paths < 500) {
    message(paste0('You have requested ', num_paths,
                   ' model evaluations. Be patient.'))
  }
  # if(num_paths >= 500 & !i_am_patient) {
  #   stop(paste0('You have requested ', num_paths,
  #               ' model evaluations but also flagged your impatience.',
  #               ' Run again with i_am_patient = TRUE'))
  # }

  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(previous_outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  } else{
    base_df <- parse_phase1_outcomes('', as_list = FALSE)
  }
  # dose <- base_df$dose
  # tox <- base_df$tox
  # cohort <- base_df$cohort
  # next_cohort <- ifelse(length(base_df$cohort) > 0, max(base_df$cohort) + 1, 1)
  # if('time' %in% colnames(base_df)) {
  #   time <- previous_outcomes$time
  # } else {
  #   time <- rep(0, length(dose))
  # }

  # if(nchar(previous_outcomes) > 0)
  #   dat <- df_parse_outcomes(previous_outcomes)
  # else
  #   dat <- list(doses = c(), tox = c(), num_patients = 0)
  # num_doses <- length(skeleton)
  # previous_doses <- base_df$dose
  # previous_tox <- base_df$tox
  # previous_num_patients <- length(base_df$dose)
  outcomes <- c('T', 'N')

  # Calculate feasible outcome combinations by cohort
  cohort_paths <- lapply(cohort_sizes,
                         function(x) combinations(n = 2, r = x,
                                                  v = outcomes,
                                                  repeats.allowed=TRUE))
  # Flatten cohort outcomes
  cohort_paths <- lapply(cohort_paths, function(x) apply(x, 1, paste0,
                                                         collapse = ''))

  # Calculate pathways
  cohort_paths <- expand.grid(cohort_paths, stringsAsFactors = FALSE)

  # Cache pathway calculations to avoid needless repetition
  cache <- list()
  # Root node is the current scenario
  root_node_id <- 1
  base_fit <- selector_factory %>% fit(base_df)
  # fit <- stan_crm(outcome_str = previous_outcomes, skeleton = skeleton,
  #                 target = target, model = model, ...)
  if(is.null(next_dose)) next_dose <- base_fit %>% recommended_dose()
  root <- dose_finding_path_node(node_id = root_node_id,
                                 parent_node_id = NA,
                                 depth = 0,
                                 outcomes = '',
                                 next_dose = next_dose,
                                 fit = base_fit,
                                 parent_fit = NULL)
  cache[['']] <- root
  node_id <- root_node_id + 1

  for(i in 1:nrow(cohort_paths)) {
    cohort_path <- cohort_paths[i, ]
    next_cohort <- ifelse(length(base_df$cohort) > 0, max(base_df$cohort) + 1, 1)
    cohort_dose <- next_dose
    pathway <- ""
    parent <- root
    fit <- root$fit

    for(j in 1:length(cohort_path)) {
      # If selector does not want to continue, this path has ended.
      if(continue(fit)) {
        pathway <- ifelse(nchar(pathway) > 0,
                      paste0(pathway, ' ', cohort_dose, cohort_path[j]),
                      paste0(cohort_dose, cohort_path[j])
        )
        if(pathway %in% names(cache)) {
          # Fetch from cache
          if(verbose) print(paste0('Fetching ', pathway, ' from cache'))
          parent <- cache[[pathway]]
          cohort_dose <- parent$next_dose
          fit <- parent$fit
        } else {
          # Fit model for path, and cache.
          these_outcomes <- parse_phase1_outcomes(pathway, as_list = FALSE)
          dat <- tibble(
            dose = array(c(base_df$dose, these_outcomes$dose)),
            tox = array(c(base_df$tox, these_outcomes$tox)),
            cohort = array(c(base_df$cohort, these_outcomes$cohort))
          )
          # dat$doses <- array(c(previous_doses, these_outcomes$dose))
          # dat$tox <- array(c(previous_tox, these_outcomes$tox))
          # dat$num_patients <- previous_num_patients +
          #   these_outcomes$num_patients
          if(verbose) print(paste0('Running ', pathway))
          fit <- selector_factory %>% fit(dat)
          cohort_dose <- recommended_dose(fit)
          # if(is.null(user_dose_func))
          #   cohort_dose <- fit$recommended_dose
          # else
          #   cohort_dose <- user_dose_func(fit)

          # Cache
          node <- dose_finding_path_node(node_id = node_id,
                                         parent_node_id = parent$.node,
                                         depth = j,
                                         outcomes = as.character(cohort_path[j]),
                                         next_dose = cohort_dose,
                                         fit = fit,
                                         parent_fit = parent$fit)
          cache[[pathway]] <- node
          parent <- node
          node_id <- node_id + 1
          next_cohort <- next_cohort + 1
        }
      }
    }
  }

  class(cache) <- c("dose_paths")
  cache
}


#' Spread the information in dose_finding_paths object to a wide data.frame format.
#'
#' @param df Optional \code{data.frame} like that returned by
#' as_tibble(dose_finding_paths). Columns .depth, .node, .parent are required.
#' All other columns are spread with a suffix reflecting depth.
#' @param dose_finding_paths Optional instance of dose_finding_paths. Required
#' if `df` is null.
#' @param max_depth integer, maximum depth of paths to traverse.
#'
#' @return A data.frame
#'
#' @importFrom tibble as_tibble
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select rename full_join rename_at vars starts_with
#'
#' @export
#'
#' @examples
#' \dontrun{
#' target <- 0.25
#' skeleton <- c(0.05, 0.15, 0.25, 0.4, 0.6)
#' paths <- crm_dtps(skeleton = skeleton, target = target, model = 'empiric',
#'                   cohort_sizes = c(1, 1), next_dose = 3, beta_sd = 1)
#' spread_paths(dose_finding_paths = paths)
#'
#' df <- as_tibble(paths)
#' spread_paths(df)
#' spread_paths(df %>% select(-fit, -parent_fit, -dose_index))
#' }
spread_paths <- function(df = NULL,
                         dose_finding_paths = NULL,
                         max_depth = NULL) {

  if(is.null(df) & is.null(dose_finding_paths))
    stop('Specify either df or dose_finding_paths.')
  if(is.null(df) & !is.null(dose_finding_paths))
    df <- as_tibble(dose_finding_paths)
  if(is.null(max_depth)) max_depth <- max(df$.depth)
  if(!all(c('.depth', '.node', '.parent') %in% colnames(df)))
    stop("Columns '.depth', '.node' and '.parent' are required.")

  .depth <- .parent <- .node <- Node <- NULL

  depth = 0
  wide_df <- df %>%
    filter(.depth == depth) %>%
    select(-.parent, -.depth) %>%
    rename_at(vars(-starts_with(".")), function(x) paste0(x, depth)) %>%
    rename(Node = .node)

  for(depth in 1:max_depth) {
    sub_df <- df %>%
      filter(.depth == depth) %>%
      rename_at(vars(-starts_with(".")), function(x) paste0(x, depth)) %>%
      select(-.depth)

    wide_df <- wide_df %>%
      full_join(sub_df,
                by = c('Node' = '.parent'),
                suffix = paste0(".", c(depth - 1, depth))) %>%
      select(-Node) %>%
      rename(Node = .node)
  }

  wide_df %>% select(-Node)
}
