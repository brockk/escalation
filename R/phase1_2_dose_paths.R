
#' @importFrom gtools combinations
phase1_2_dose_paths <- function(
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

  if(is.character(previous_outcomes)) {
    base_df <- parse_phase1_2_outcomes(previous_outcomes, as_list = FALSE)
  } else if(is.data.frame(previous_outcomes)) {
    base_df <- spruce_outcomes_df(previous_outcomes)
  } else{
    base_df <- parse_phase1_2_outcomes('', as_list = FALSE)
  }

  # Calculate feasible outcome combinations by cohort
  outcomes <- c('E', 'T', 'N', 'B')
  cohort_paths <- lapply(
    cohort_sizes, function(x) combinations(n = length(outcomes),
                                           r = x,
                                           v = outcomes,
                                           repeats.allowed = TRUE))
  # Flatten cohort outcomes
  cohort_paths <- lapply(cohort_paths, function(x) apply(x, 1, paste0,
                                                         collapse = ''))

  # Calculate pathways
  cohort_paths <- expand.grid(cohort_paths, stringsAsFactors = FALSE)
  num_paths = nrow(cohort_paths)
  if(num_paths >= 100) {
    message(paste0('You have requested ', num_paths, ' paths. Be patient.'))
  }

  # Cache pathway calculations to avoid needless repetition
  cache <- list()
  # Root node is the current scenario
  root_node_id <- 1
  base_fit <- selector_factory %>% fit(base_df)
  if(is.null(next_dose)) next_dose <- base_fit %>% recommended_dose()
  root <- dose_finding_path_node(
    node_id = root_node_id,
    parent_node_id = NA,
    depth = 0,
    outcomes = previous_outcomes,
    next_dose = next_dose,
    fit = base_fit,
    parent_fit = NULL)
  cache[['']] <- root
  node_id <- root_node_id + 1

  for(i in 1:nrow(cohort_paths)) {
    cohort_path <- cohort_paths[i, ]
    next_cohort <- ifelse(length(base_df$cohort) > 0,
                          max(base_df$cohort) + 1,
                          1)
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
          these_outcomes <- parse_phase1_2_outcomes(pathway, as_list = FALSE)
          dat <- tibble(
            dose = array(c(base_df$dose, these_outcomes$dose)),
            tox = array(c(base_df$tox, these_outcomes$tox)),
            eff = array(c(base_df$eff, these_outcomes$eff)),
            cohort = array(c(base_df$cohort, these_outcomes$cohort))
          )
          if(verbose) print(paste0('Running ', pathway))
          fit <- selector_factory %>% fit(dat)
          cohort_dose <- recommended_dose(fit)
          # Cache
          node <- dose_finding_path_node(
            node_id = node_id,
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
