
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

# Run sims ----
set.seed(2023)
dependent <- simulate_compare(designs,
                              num_sims = num_sims,
                              true_prob_tox,
                              true_prob_eff)
ls(dependent)
class(dependent)
dependent[[1]]
dependent$`BOIN12 v1`
dependent[[2]]
dependent[[3]]

library(purrr)
independent <- designs %>%
  imap(
    ~ {
      cat("Running", .y, "\n")
      .x %>%
      simulate_trials(num_sims = num_sims,
                      true_prob_tox,
                      true_prob_eff)
    }
  ) %>%
      make_simulations_collection()
ls(independent)
class(independent)
independent[[1]]
independent[[2]]
independent[[3]]

# Post-process ----
convergence_plot(dependent)
convergence_plot(independent)

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
  mutate(series = forcats::fct_inorder(series)) %>%
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
