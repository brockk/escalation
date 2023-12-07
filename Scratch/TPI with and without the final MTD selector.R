
library(escalation)
library(dplyr)
library(ggplot2)

# Load ----
target <- 0.3

tpi_design <- get_tpi(num_doses = 8, target = target, k1 = 1, k2 = 1.5,
                      exclusion_certainty = 0.95) %>%
  stop_at_n(n = 30)
tpi_with_iso <- tpi_design %>%
  select_tpi_mtd(exclusion_certainty = 0.95)
designs <- list(
  "TPI" = tpi_design,
  "TPI plus isotonic" = tpi_with_iso
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
# final MTD selector is significantly more like to select dose 2 over 1 & 3
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

sc2$TPI
sc2$`TPI plus isotonic`

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
# Designs are similar in this scenario.

sc3$TPI

# Sc 4 ----
set.seed(2023)
sc4 = simulate_compare(
  designs,
  num_sims = 10^2,
  true_prob_tox = c(0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.99)
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
# A bit more likely to select dose 1 over 2, but not yet sig.


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
# final MTD selector is significantly more like to select dose 1 over dose 3 & 4
# in this scenario, buty similar at dose 2.

sc5$`TPI plus isotonic`


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
# final MTD selector is significantly more like to select doses 2 over
# higher doses in this scenario.
