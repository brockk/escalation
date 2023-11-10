
# Load ----
library(escalation)
# library(dplyr)
# library(purrr)
# library(ggplot2)
# library(tidyr)
# library(forcats)

# num_sims <- 10^4
num_sims <- 10^3
num_doses <- 5

# OBD-seeking
true_prob_tox <- c(0.05, 0.10, 0.15, 0.18, 0.45)
true_prob_eff <- c(0.40, 0.50, 0.52, 0.53, 0.53)

# Mike's BOIN12 example
designs <- list(

  "BOIN12 v1" = get_boin12(num_doses = num_doses,
                          phi_t = 0.35, phi_e = 0.25,
                          u2 = 40, u3 = 60,
                          c_t = 0.95, c_e = 0.9) %>%
    stop_at_n(n = 36),

  "BOIN12 v2" = get_boin12(num_doses = num_doses,
                          phi_t = 0.35, phi_e = 0.25,
                          u2 = 40, u3 = 60,
                          c_t = 0.85, c_e = 0.8
                          ) %>%
    stop_at_n(n = 36)
)

# # OBD seeking
# designs <- list(
#   # EffTox vs W&T vs BOIN12 TODO
# )
#
# # MTD-seeking
# true_prob_tox <- c(0.05, 0.10, 0.15, 0.18, 0.45)
# tox_target <- 0.25
# skeleton <- dfcrm::getprior(halfwidth = 0.1, target = tox_target,
#                             nlevel = num_doses, nu = 2)
# designs <- list(
#   "3+3" = get_three_plus_three(num_doses = num_doses),
#   "BOIN" = get_boin(num_doses = num_doses, target = tox_target) %>%
#     stop_at_n(n = 12),
#   "CRM" = get_dfcrm(skeleton = skeleton, target = tox_target) %>%
#     dont_skip_doses() %>%
#     stop_at_n(n = 12)
# )

# New functions ----
#'
#' #' Simulate clinical trials.
#' #'
#' #' @description This function takes a \code{\link{selector_factory}}, such as
#' #'   that returned by \code{\link{get_dfcrm}}, \code{\link{get_boin}} or
#' #'   \code{\link{get_three_plus_three}}, and conducts many notional clinical
#' #'   trials. We conduct simulations to learn about the operating characteristics
#' #'   of adaptive trial designs.
#' #'
#' #' @details By default, dose decisions in simulated trials are made after each
#' #'   cohort of 3 patients. This can be changed by providing a function by the
#' #'   \code{sample_patient_arrivals} parameter that simulates the arrival of new
#' #'   patients. The new patients will be added to the existing patients and the
#' #'   model will be fit to the set of all patients. The function that simulates
#' #'   patient arrivals should take as a single parameter a data-frame with one
#' #'   row for each existing patient and columns including cohort, patient, dose,
#' #'   tox, time (and possibly also eff and weight, if a phase I/II or
#' #'   time-to-event method is used). The provision of data on the existing
#' #'   patients allows the patient sampling function to be adaptive. The function
#' #'   should return a data-frame with a row for each new patient and a column for
#' #'   time_delta, the time between the arrival of this patient and the previous,
#' #'   as in \code{\link{cohorts_of_n}}. See Examples.
#' #'
#' #'   This method can simulate the culmination of trials that are partly
#' #'   completed. We just have to specify the outcomes already observed via the
#' #'   \code{previous_outcomes} parameter. Each simulated trial will commence from
#' #'   those outcomes seen thus far. See Examples.
#' #'
#' #'   We can specify the immediate next dose by specifying \code{next_dose}. If
#' #'   omitted, the next dose is calculated by invoking the model on the outcomes
#' #'   seen thus far.
#' #'
#' #'   Designs must eventually choose to stop the trial. Some designs, like 3+3,
#' #'   have intrinsic stopping rules. However, some selectors like those derived
#' #'   from \code{\link{get_dfcrm}} offer no default stopping method. You may need
#' #'   to append stopping behaviour to your selector via something like
#' #'   \code{\link{stop_at_n}} or \code{\link{stop_when_n_at_dose}}, etc. To
#' #'   safeguard against simulating runaway trials that never end, the function
#' #'   will halt a simulated trial after 30 invocations of the dose-selection
#' #'   decision. To breach this limit, specify \code{i_like_big_trials = TRUE} in
#' #'   the function call. However, when you forego the safety net, the onus is on
#' #'   you to write selectors that will eventually stop the trial! See Examples.
#' #'
#' #'   The model is fit to the prevailing data at each dose selection point. By
#' #'   default, only the final model fit for each simulated trial is retained.
#' #'   This is done to conserve memory. With a high number of simulated trials,
#' #'   storing many model fits per trial may cause the executing machine to run
#' #'   out of memory. However, you can force this method to retain all model fits
#' #'   by specifying \code{return_all_fits = TRUE}. See Examples.
#' #'
#' #' @param selector_factory Object of type \code{\link{selector_factory}}.
#' #' @param num_sims integer, number of trial iterations to simulate.
#' #' @param true_prob_tox numeric vector of true but unknown toxicity
#' #'   probabilities
#' #' @param true_prob_eff numeric vector of true but unknown efficacy
#' #'   probabilities. NULL if efficacy not analysed.
#' #' @param ... Extra args are passed onwards.
#' #'
#' #' @seealso \code{\link{simulations}}
#' #' @seealso \code{\link{selector_factory}}
#' #' @seealso \code{\link{get_dfcrm}}
#' #' @seealso \code{\link{get_boin}}
#' #' @seealso \code{\link{get_three_plus_three}}
#' #' @seealso \code{\link{cohorts_of_n}}
#' #'
#' #' @return list mapping design name to \code{\link{simulations}} object
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' # In a five-dose scenario, we have assumed probabilities for Prob(tox):
#' #' true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
#' #'
#' #' # Simulate ten 3+3 trials:
#' #' sims <- get_three_plus_three(num_doses = 5) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)
#' #' # Likewise, simulate 10 trials using a continual reassessment method:
#' #' skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
#' #' target <- 0.25
#' #' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' #'   stop_at_n(n = 12) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)
#' #'
#' #' # Lots of useful information is contained in the returned object:
#' #' sims %>% num_patients()
#' #' sims %>% num_doses()
#' #' sims %>% dose_indices()
#' #' sims %>% n_at_dose()
#' #' sims %>% n_at_recommended_dose()
#' #' sims %>% tox_at_dose()
#' #' sims %>% num_tox()
#' #' sims %>% recommended_dose()
#' #' sims %>% prob_administer()
#' #' sims %>% prob_recommend()
#' #' sims %>% trial_duration()
#' #'
#' #' # By default, dose decisions are made after each cohort of 3 patients. See
#' #' # Details. To override, specify an alternative function via the
#' #' # sample_patient_arrivals parameter. E.g. to use cohorts of 2, we run:
#' #' patient_arrivals_func <- function(current_data) cohorts_of_n(n = 2)
#' #' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' #'   stop_at_n(n = 12) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#' #'     sample_patient_arrivals = patient_arrivals_func)
#' #'
#' #' # To simulate the culmination of trials that are partly completed, specify
#' #' # the outcomes already observed via the previous_outcomes parameter. Imagine
#' #' # one cohort has already been evaluated, returning outcomes 1NTN. We can
#' #' # simulate the remaining part of the trial with:
#' #' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' #'   stop_at_n(n = 12) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#' #'                   previous_outcomes = '1NTN')
#' #' # Outcomes can be described by the above outcome string method or data-frame:
#' #'   previous_outcomes <- data.frame(
#' #'     patient = 1:3,
#' #'     cohort = c(1, 1, 1),
#' #'     tox = c(0, 1, 0),
#' #'     dose = c(1, 1, 1)
#' #'   )
#' #' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' #'   stop_at_n(n = 12) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#' #'                   previous_outcomes = previous_outcomes)
#' #'
#' #' # We can specify the immediate next dose:
#' #' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' #'   stop_at_n(n = 12) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#' #'                   next_dose = 5)
#' #'
#' #' # By default, the method will stop simulated trials after 30 dose selections.
#' #' # To suppress this, specify i_like_big_trials = TRUE. However, please take
#' #' # care to specify selectors that will eventually stop!
#' #' sims <- get_dfcrm(skeleton = skeleton, target = target) %>%
#' #'   stop_at_n(n = 99) %>%
#' #'   simulate_trials(num_sims = 1, true_prob_tox = true_prob_tox,
#' #'                   i_like_big_trials = TRUE)
#' #'
#' #' # By default, only the final model fit is retained for each simulated trial.
#' #' # To retain all interim model fits, specify return_all_fits = TRUE.
#' #' sims <- get_three_plus_three(num_doses = 5) %>%
#' #'   simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox,
#' #'                   return_all_fits = TRUE)
#' #' # Verify that there are now many analyses per trial with:
#' #' sapply(sims$fits, length)
#' simulate_compare <- function(
#'     designs,
#'     num_sims,
#'     true_prob_tox,
#'     true_prob_eff = NULL,
#'     ...) {
#'
#'   patient_samples <- lapply(1:num_sims, function(x) PatientSample$new())
#'   out <- list()
#'   for(label in ls(designs)) {
#'     cat("Running", label, "\n")
#'     design <- designs[[label]]
#'     sim_func <- simulation_function(design)
#'     if(is.null(true_prob_eff)) {
#'       l <- lapply(
#'         1:num_sims,
#'         function(i) {
#'           sim_func(design,
#'                    true_prob_tox = true_prob_tox,
#'                    patient_sample = patient_samples[[i]],
#'           ...)
#'         }
#'       )
#'     } else {
#'       l <- lapply(
#'         1:num_sims,
#'         function(i) {
#'           sim_func(design,
#'                    true_prob_tox = true_prob_tox,
#'                    true_prob_eff = true_prob_eff,
#'                    patient_sample = patient_samples[[i]],
#'           ...)
#'         }
#'       )
#'     }
#'
#'     sims <- simulations(
#'       fits = l,
#'       true_prob_tox = true_prob_tox,
#'       true_prob_eff = true_prob_eff
#'     )
#'     out[[label]] <- sims
#'   }
#'
#'   return(make_simulations_collection(out))
#' }
#'
#' #' Make an instance of type \code{simulations_collection}
#' #'
#' #' @param sim_map list, character -> \class{\link{simulations}} object
#' #'
#' #' @return object of class \code{simulations_collection}, inheriting from list
#' #' @export
#' #'
#' #' @examples
#' #' # TODO
#' make_simulations_collection <- function(sim_map) {
#'   class(sim_map) <- c("simulations_collection", class(sim_map))
#'   return(sim_map)
#' }
#'
#' #' Stack \code{\link{simulations_collection}} results vertically
#' #'
#' #' @param sim_map object of type \code{\link{simulations_collection}}
#' #' @param target_dose optional integer vector, the dose of interest. All doses
#' #'          are analysed if omitted, which is the default.
#' #' @param alpha confidence level for asymptotic normal confidence intervals. The
#' #' default value is 0.05 to get 95% confidence intervals.
#' #'
#' #' @return \code{data.frame}
#' #'
#' #' @importFrom magrittr %>%
#' #' @importFrom purrr map imap reduce
#' #' @importFrom dplyr tibble bind_rows mutate
#' #' @export
#' #'
#' #' @examples
#' #' # TODO
#' stack_sims_vert <- function(sim_map, target_dose = NULL, alpha = 0.05) {
#'   q <- qnorm(p = alpha / 2, lower.tail = FALSE)
#'   sim_map %>%
#'     imap(
#'       ~ {
#'         if(is.null(target_dose)) target_dose <- dose_indices(.x)
#'         rec_d <- recommended_dose(.x)
#'         rec_d[is.na(rec_d)] <- 0 # Replace NAs with 0
#'
#'         target_dose %>%
#'           map(.f = function(td) {
#'             tibble(
#'               dose = td,
#'               hit = rec_d == td,
#'               r = cumsum(hit),
#'               n = seq_len(length(.x))
#'             )
#'           }) %>%
#'           reduce(bind_rows) %>%
#'           mutate(design = .y)
#'
#'       }
#'     ) %>%
#'     reduce(bind_rows) %>%
#'     mutate(
#'       .rate = r / n,
#'       .se = sqrt(.rate * (1 - .rate) / n),
#'       .l = .rate - q * .se,
#'       .u = .rate + q * .se
#'     )
#' }
#'
#' #' Compare the probabilities of dose selection in several simulation sets.
#' #'
#' #' Normal approximation in derivation of confidence intervals of differences in
#' #' probability of selection. Details are in Sweeting et al.
#' #'
#' #' @inheritParams stack_sims_vert
#' #'
#' #' @return \code{data.frame}
#' #'
#' #' @importFrom magrittr %>%
#' #' @importFrom dplyr inner_join select mutate filter group_by ungroup
#' #' @export
#' #'
#' #' @examples
#' #' # TODO
#' #'
#' #' @references
#' #' # TODO
#' compare_sims <- function(sim_map, target_dose = NULL, alpha = 0.05) {
#'   q <- qnorm(p = alpha / 2, lower.tail = FALSE)
#'   stacked_df <- stack_sims_vert(sim_map, target_dose, alpha = alpha)
#'   # Compare each design to every other design:
#'   inner_join(
#'     stacked_df %>%
#'       select(dose, n, design, hit) %>%
#'       mutate(design = ordered(design)),
#'     stacked_df %>%
#'       select(dose, n, design, hit) %>%
#'       mutate(design = ordered(design)),
#'     by = c("dose", "n"),
#'     relationship ="many-to-many"
#'   ) %>%
#'     # Discard self-comparisons and replicates:
#'     filter(design.x > design.y) %>%
#'     group_by(dose) %>%
#'     mutate(
#'       X = cumsum(hit.x),
#'       X2 = cumsum(hit.x^2),
#'       Y = cumsum(hit.y),
#'       Y2 = cumsum(hit.y^2),
#'       XY = cumsum(hit.x * hit.y),
#'       psi1 = X / n,
#'       psi2 = Y / n,
#'       v_psi1 = (X2 / n - (X / n)^2) / n,
#'       v_psi2 = (Y2 / n - (Y / n)^2) / n,
#'       cov_psi12 = (XY / n - (X / n) * (Y / n)) / n,
#'       delta = psi1 - psi2,
#'       v_delta = v_psi1 + v_psi2 - 2 * cov_psi12,
#'       se_delta = sqrt(v_delta),
#'       delta_l = delta - q * se_delta,
#'       delta_u = delta + q * se_delta,
#'       comparison = paste0(design.x, " vs ", design.y)
#'     ) %>%
#'     ungroup()
#' }
#'
#' # class(independent_obd)
#' # class(independent_obd[[1]])
#' # x = independent_obd
#' convergence_plot <- function(x) {
#'   num_doses(x[[1]])
#'   stack_sims_vert(x, target_dose = 2)
#'   cum_sum_df <- stack_sims_vert(x, target_dose = 2)
#'   # TODO
#' }


# Run sims ----
set.seed(2023)
# dependent <- simulate_compare(designs,
#                               num_sims = num_sims,
#                               true_prob_tox)
dependent <- simulate_compare(designs,
                              num_sims = num_sims,
                              true_prob_tox,
                              true_prob_eff)
ls(dependent)
class(dependent)
# dependent[["BOIN12 v1"]]
dependent[[1]]
# dependent[["BOIN12 v2"]]
dependent[[2]]
dependent[[3]]

independent <- designs %>%
  imap(
    ~ {
      cat("Running", .y, "\n")
      .x %>%
        # simulate_trials(num_sims = num_sims,
        #                 true_prob_tox)
      simulate_trials(num_sims = num_sims,
                      true_prob_tox,
                      true_prob_eff)
    }
  ) %>%
      make_simulations_collection()
ls(independent)
class(independent)
# independent[["BOIN12 v1"]]
independent[[1]]
# independent[["BOIN12 v2"]]
independent[[2]]
independent[[3]]

# Post-process ----
recommended_dose(dependent[[1]])
recommended_dose(dependent[[2]])
sum(recommended_dose(dependent[[1]]) != recommended_dose(dependent[[2]]),
    na.rm = TRUE)
sum(recommended_dose(independent[[1]]) != recommended_dose(independent[[2]]),
    na.rm = TRUE)

cum_sum_df_dep <- stack_sims_vert(dependent)
cum_sum_df_dep %>%
  ggplot(aes(x = n, y = .rate, col = design)) +
  geom_line() +
  ylim(0, 1) +
  facet_wrap(~ dose, ncol = 5) +
  labs(title = "Convergence of simulation",
       subtitle = "Shared patients",
       x = "Iterate", y = "Prob(Selection)") +
  theme(legend.position = "bottom") -> p1; p1
cum_sum_df_indep <- stack_sims_vert(independent)
cum_sum_df_indep %>%
  ggplot(aes(x = n, y = .rate, col = design)) +
  geom_line() +
  ylim(0, 1) +
  facet_wrap(~ dose, ncol = 5) +
  labs(title = "Convergence of simulation",
       subtitle = "Independent patients",
       x = "Iterate", y = "Prob(Selection)") +
  theme(legend.position = "bottom") -> p2; p2

# Convergence of PCS
bind_rows(
  cum_sum_df_dep %>%
    mutate(series = paste0(design, " (Set 1 simulations)")),
  cum_sum_df_indep %>%
    filter(design == "BOIN12 v2") %>%
    mutate(series = paste0(design, " (Set 2 simulations)")),
) %>%
  mutate(series = fct_inorder(series)) %>%
  ggplot(aes(x = n, y = .rate, col = series)) +
  geom_line() +
  # coord_cartesian(ylim = c(0.25, 0.4)) +
  facet_wrap(~ dose, ncol = 5) +
  theme(legend.position = "bottom")
# c.f. Figure 2 in manuscript

# Contrast methods
# contrasts_df2 <- bind_rows(
#   compare_sims(dependent) %>% mutate(sims = "paired"),
#   compare_sims(independent) %>% mutate(sims = "independent")
# )
contrasts_df2 <- bind_rows(
  as_tibble(dependent) %>% mutate(sims = "paired"),
  as_tibble(independent) %>% mutate(sims = "independent")
)

contrasts_df2 %>%
  filter(v_psi1 < 0 | v_psi2 < 0) # Problems? TODO
  # filter(se_delta %>% is.na) %>%
  # head(3) %>% t
# contrasts_df2 %>%
#
#   filter(sims == "paired") %>%
#   # filter(sims == "independent") %>%
#   filter(dose == 2) %>%
#
#   filter(n %% 10 == 0) %>%
#   # select(-hit.x, -hit.y, -X, -X2, -Y, -Y2, -XY) %>%
#   ggplot(aes(x = n, y = delta)) +
#   geom_point(size = 0.4) +
#   geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
#   geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
#   facet_grid(~ dose, labeller = labeller(.cols = label_both))

contrasts_df2 %>%
  filter(n %% 50 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison + sims ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
# c.f. Figure 3 in manuscript

contrasts_df2 %>%
  count(sims, dose, design.x, design.y)

contrasts_df2 %>%
  select(sims, dose, n, v_delta, comparison) %>%
  spread(sims, v_delta) %>%
  filter(paired > 0 & independent > 0) %>%
  mutate(ess_scaler = independent / paired) %>%
  ggplot(aes(x = n, y = ess_scaler, group = dose)) +
  geom_line() +
  ylim(0, NA) +
  facet_wrap(~ comparison)

contrasts_df2 %>%
  select(sims, dose, n, v_delta, comparison) %>%
  spread(sims, v_delta) %>%
  filter(paired > 0 & independent > 0) %>%
  mutate(ess_scaler = independent / paired) %>%
  ggplot(aes(x = ess_scaler)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 1, linetype = "dashed", col = "red") +
  # facet_wrap(~ dose, labeller = labeller(.cols = label_both)) +
  facet_grid(comparison ~ dose, labeller = labeller(.cols = label_both)) +
  labs(x = "MC error shrinkage scalar (1 = no shrinkage)")

# # Convergences of method 1 - method 2
# contrasts_df <- bind_rows(
#
#   # Compare each design to every other design:
#   inner_join(
#     cum_sum_df_dep %>%
#       select(design, dose, n, hit, .rate, .se) %>%
#       mutate(design = ordered(design)),
#     cum_sum_df_dep %>%
#       select(design, dose, n, hit, .rate, .se) %>%
#       mutate(design = ordered(design)),
#     by = c("dose", "n"),
#     relationship ="many-to-many"
#   ) %>%
#     # Discard self-comparisons and replicates:
#     filter(design.x > design.y) %>%
#     mutate(
#       diff = .rate.x - .rate.y,
#       diff_se = sqrt(.se.x^2 + .se.y^2) # + covar!
#     ) %>%
#     mutate(method = "Paired"),
#
#   inner_join(
#     cum_sum_df_indep %>%
#       select(design, dose, n, hit, .rate, .se) %>%
#       mutate(design = ordered(design)),
#     cum_sum_df_indep %>%
#       select(design, dose, n, hit, .rate, .se) %>%
#       mutate(design = ordered(design)),
#     by = c("dose", "n"),
#     relationship ="many-to-many"
#   ) %>%
#     filter(design.x > design.y) %>%
#     mutate(
#       diff = .rate.x - .rate.y,
#       # diff_se = sqrt(.se.x^2 + .se.y^2) # TODO covar!
#     ) %>%
#     mutate(method = "Independent")
#
# )
#
# contrasts_df %>%
#   ggplot(aes(x = n, y = diff, col = method)) +
#   geom_line() +
#   facet_wrap(~ dose) +
#   ylim(-0.1, 0.1) +
#   theme(legend.position = "bottom") +
#   labs(title = "Convergence of prob(select)")
# contrasts_df %>%
#   filter(dose == 2) %>%
#   ggplot(aes(x = n, y = diff, col = method)) +
#   geom_line() +
#   facet_wrap(~ dose) +
#   ylim(-0.1, 0.1) +
#   theme(legend.position = "bottom") +
#   labs(title = "Convergence of prob(select)")
# contrasts_df %>%
#   # filter(method == "Paired") %>%
#   filter(n == 100) %>%
#   mutate(
#     z = diff / diff_se,
#     z.p = pnorm(abs(z), lower.tail = FALSE)
#   ) %>%
#   select(-design.x, -design.y)
#
# contrasts_df %>%
#   filter(dose == 2) %>%
#   filter(n == 1000)
#
# contrasts_df %>%
#   filter(dose == 2) %>%
#   filter(method == "Paired") %>%
#   select(.rate.x, .rate.y)


# Removed ----

# Rehoused from Scratch.R
#
# # Michael's convergence plot
# library(escalation)
# library(dplyr)
# library(ggplot2)
#
# model <- get_mtpi(num_doses = 8, target = 0.25,
#                   epsilon1 = 0.05, epsilon2 = 0.05,
#                   exclusion_certainty = 0.95) %>%
#   stop_at_n(n = 30)
#
# # Scenario 1
# sc1 <- c(0.05, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
# set.seed(123)
# sims1 <- model %>%
#   simulate_trials(num_sims = 1000, true_prob_tox = sc1, next_dose = 1)
#
# target_dose <- 2
# tibble(
#   r = cumsum(recommended_dose(sims1) == target_dose),
#   n = seq_len(length(sims1))
# ) %>%
#   mutate(
#     .rate = r / n,
#     .se = sqrt(.rate * (1 - .rate) / n),
#     .l = .rate - 1.96 * .se,
#     .u = .rate + 1.96 * .se
#   ) %>%
#   ggplot(aes(x = n, y = .rate)) +
#   geom_line() +
#   ylim(0, 1) +
#   labs(title = "Convergence of simulation",
#        x = "Iterate", y = "Prob(CorrectSelection)")

# From above
# # MTD-seeking
# true_prob_tox <- c(0.05,0.10,0.15,0.18,0.45)
# tox_target <- 0.25
# skeleton <- dfcrm::getprior(halfwidth = 0.1, target = tox_target,
#                             nlevel = num_doses, nu = 2)
#
# designs_mtd <- list(
#   "3+3" = get_three_plus_three(num_doses = num_doses),
#   "BOIN" = get_boin(num_doses = num_doses, target = tox_target) %>%
#     stop_at_n(n = 12),
#   "CRM" = get_dfcrm(skeleton = skeleton, target = tox_target) %>%
#             dont_skip_doses() %>%
#             stop_at_n(n = 12)
# )
# set.seed(2023)
# dependent_mtd <- simulate_trials_new(designs_mtd,
#                                      num_sims = num_sims,
#                                      true_prob_tox,
#                                      true_prob_eff = NULL)
# independent_mtd <- designs_mtd %>%
#   imap(
#     ~ {
#       cat("Running", .y, "\n")
#       .x %>%
#         simulate_trials(num_sims = num_sims,
#                         true_prob_tox,
#                         true_prob_eff = NULL)
#     }
#   )
#
# recommended_dose(dependent_mtd$`3+3`) # Note the NAs
# cum_sum_df_dep <- stack_sims_vert(dependent_mtd, target_dose = 2)
# cum_sum_df_dep %>%
#   ggplot(aes(x = n, y = .rate, col = design)) +
#   geom_line() +
#   ylim(0, 1) +
#   labs(title = "Convergence of simulation",
#        subtitle = "Shared patients",
#        x = "Iterate", y = "Prob(CorrectSelection)") +
#   theme(legend.position = "none") -> p1; p1
# cum_sum_df_indep <- stack_sims_vert(independent_mtd, target_dose = 2)
# cum_sum_df_indep %>%
#   ggplot(aes(x = n, y = .rate, col = design)) +
#   geom_line() +
#   ylim(0, 1) +
#   labs(title = "Convergence of simulation",
#        subtitle = "Independent patients",
#        x = "Iterate", y = "Prob(CorrectSelection)")  +
#   theme(legend.position = "none") -> p2; p2
# p1 + p2

# eff_skeletons = matrix(nrow=9, ncol=5)
# eff_skeletons[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20)
# eff_skeletons[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30)
# eff_skeletons[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40)
# eff_skeletons[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50)
# eff_skeletons[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60)
# eff_skeletons[6,] <- c(0.30, 0.40, 0.50, 0.60, 0.60)
# eff_skeletons[7,] <- c(0.40, 0.50, 0.60, 0.60, 0.60)
# eff_skeletons[8,] <- c(0.50, 0.60, 0.60, 0.60, 0.60)
# eff_skeletons[9,] <- c(0.60, 0.60, 0.60, 0.60, 0.60)
# eff_skeleton_weights = rep(1, nrow(eff_skeletons))
# wt1 <- get_wages_and_tait(tox_skeleton = skeleton,
#                           eff_skeletons = eff_skeletons,
#                           tox_limit = 0.3, eff_limit = 0.25,
#                           num_randomise = 20) %>%
#   stop_at_n(n = 36)
# # wt1 %>% simulate_trials(num_sims = 10,
# #                         true_prob_tox = true_prob_tox,
# #                         true_prob_eff = true_prob_eff)
# wt2 <- get_wages_and_tait(tox_skeleton = skeleton,
#                           eff_skeletons = eff_skeletons,
#                           tox_limit = 0.4, eff_limit = 0.15,
#                           num_randomise = 20) %>%
#   stop_at_n(n = 36)
# designs_obd <- list(
#   "WT1" = wt1,
#   "WT2" = wt2
# )

# cum_sum_df_dep %>% count(dose, design)
# cum_sum_df_dep %>% head(10)
# test_df1 <- cum_sum_df_dep %>%
#   filter(dose == 2 & design == "BOIN12 v1")
# test_df2 <- cum_sum_df_dep %>%
#   filter(dose == 2 & design == "BOIN12 v2")
#
# # # Version 1
# # test_df1 %>%
# #   inner_join(test_df2, by = c("dose", "n")) %>%
# #   select(X = hit.x, Y = hit.y) %>%
# #   summarise(
# #     psi1 = mean(X),
# #     psi2 = mean(Y),
# #     v_psi1 = var(X) / length(X),
# #     v_psi2 = var(Y) / length(Y),
# #     cov_psi12 = cov(X, Y) / length(X),
# #   ) %>%
# #   mutate(
# #     delta = psi1 - psi2,
# #     v_delta = v_psi1 + v_psi2 - 2 * cov_psi12,
# #     se_delta = sqrt(v_delta),
# #     delta_l = delta - 1.96 * se_delta,
# #     delta_u = delta + 1.96 * se_delta,
# #   )
#
# # # Version 2
# # test_df1 %>%
# #   mutate(
# #     X = cumsum(hit),
# #     X2 = cumsum(hit^2),
# #     Var = (X2 / n - (X / n)^2) / n
# #   ) %>%
# #   select(-dose, -design, -.l, -.u) %>%
# #   tail(8)
# # test_df2 %>%
# #   mutate(
# #     X = cumsum(hit),
# #     X2 = cumsum(hit^2),
# #     Var = (X2 / n - (X / n)^2) / n
# #   ) %>%
# #   select(-dose, -design, -.l, -.u) %>%
# #   tail(8)
#
# # Version 3
# contrasts_df <-
#   test_df1 %>% select(dose, n, design, hit) %>%
#   inner_join(test_df2 %>% select(dose, n, design, hit),
#              by = c("dose", "n")) %>%
#   mutate(
#     X = cumsum(hit.x),
#     X2 = cumsum(hit.x^2),
#     Y = cumsum(hit.y),
#     Y2 = cumsum(hit.y^2),
#     XY = cumsum(hit.x * hit.y),
#     psi1 = X / n,
#     psi2 = Y / n,
#     v_psi1 = (X2 / n - (X / n)^2) / n,
#     v_psi2 = (Y2 / n - (Y / n)^2) / n,
#     cov_psi12 = (XY / n - (X / n) * (Y / n)) / n,
#     delta = psi1 - psi2,
#     v_delta = v_psi1 + v_psi2 - 2 * cov_psi12,
#     se_delta = sqrt(v_delta),
#     delta_l = delta - 1.96 * se_delta,
#     delta_u = delta + 1.96 * se_delta,
#   )
# contrasts_df
# contrasts_df %>%
#   filter(n %% 10 == 0) %>%
#   # select(-hit.x, -hit.y, -X, -X2, -Y, -Y2, -XY) %>%
#   ggplot(aes(x = n, y = delta)) +
#   geom_point(size = 0.4) +
#   geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
#   geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
#   facet_grid(~ dose, labeller = labeller(.cols = label_both))
