
library(escalation)
library(dplyr)
library(ggplot2)

# Load ----
target <- 0.25
mtpi_design <- get_mtpi(num_doses = 8, target = target, epsilon1 = 0.05,
                 epsilon2 = 0.05, exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)
mtpi_with_stopper <- mtpi_design %>%
  select_mtpi_mtd()
designs <- list(
  "mTPI" = mtpi_design,
  "mTPI with stopper" = mtpi_with_stopper
)

# Sc 1 ----
set.seed(2023)
sc1 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.05, 0.25, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)

as_tibble(sc1) %>%
  filter(n %% 10 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
as_tibble(sc1) %>%
  filter(n == 100) %>%
  select(design.x, design.y, delta_l, delta, delta_u)
# Compared to a design without the final MTD selector, the design with the
# final MTD selector is significantly more like to select dose 1 over 2 & 3
# in this scenario.

# Sc 2 ----
set.seed(2023)
sc2 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.25, 0.50, 0.60)
)

as_tibble(sc2) %>%
  filter(n %% 10 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
# Compared to a design without the final MTD selector, the design with the
# final MTD selector is significantly more like to select dose 5 over higher
# doses in this scenario.

# Sc 3 ----
set.seed(2023)
sc3 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.01, 0.05, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
)

as_tibble(sc3) %>%
  filter(n %% 10 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
# Compared to a design without the final MTD selector, the design with the
# final MTD selector is significantly more like to select dose 2 over dose 3 in
# this scenario.

# Sc 4 ----
set.seed(2023)
sc4 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.4, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.99)
)

as_tibble(sc4) %>%
  filter(n %% 10 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
# Identical. TODO - show stopping prob?

# Sc 5 ----
set.seed(2023)
sc5 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85)
)

as_tibble(sc5) %>%
  filter(n %% 10 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
# Compared to a design without the final MTD selector, the design with the
# final MTD selector is significantly more like to select dose 1 over dose 3 in
# this scenario, buty similar at dose 2.

# Sc 6 ----
set.seed(2023)
sc6 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75)
)

as_tibble(sc6) %>%
  filter(n %% 10 == 0) %>%
  ggplot(aes(x = n, y = delta)) +
  geom_point(size = 0.4) +
  geom_linerange(aes(ymin = delta_l, ymax = delta_u)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  facet_grid(comparison ~ dose,
             labeller = labeller(
               .rows = label_both,
               .cols = label_both)
  )
# Compared to a design without the final MTD selector, the design with the
# final MTD selector is significantly more like to select doses 1 & 2 over
# higher doses in this scenario.
