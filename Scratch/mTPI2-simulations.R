
library(escalation)
library(tidyverse)

# Check sims in mTPI2 paper ----
target <- 0.3
epsilon1 <- 0.05
epsilon2 <- 0.05

design <- get_mtpi2(num_doses = 6, target = target,
                    epsilon1 = epsilon1, epsilon2 = epsilon2,
                    exclusion_certainty = 0.95,
                    alpha = 1, beta = 1) %>%
  stop_at_n(n = 30) %>%
  select_mtpi2_mtd(when = "finally", exclusion_certainty = 0.95)

# Sc1 of Guo et al ----
true_prob_tox = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.25)
mtd <- 6

file_loc <- "Scratch/mTPI2/GuoScenario1.rds"
if(file.exists(file_loc)) {
  sims <- readRDS(file = file_loc)
} else {
  set.seed(2023)
  sims <- simulate_trials(
    design,
    num_sims = 10^4,
    true_prob_tox = true_prob_tox
  )
  saveRDS(sims, file = file_loc)
}
pr <- prob_recommend(sims)
pa <- prob_administer(sims)

mtd_equiv_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox >= target - epsilon1 & true_prob_tox <= target + epsilon2
]
mtd_dose_names <- unique(c(mtd_equiv_dose_names, as.character(mtd)))
safe_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox <= target + epsilon2
]
mtd_dose_names %>% map_dbl(~ pr[[.x]]) %>% sum()
safe_dose_names %>% map_dbl(~ pa[[.x]]) %>% sum()

# Without final MTD selector?
design_ <- get_mtpi2(num_doses = 6, target = 0.3,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95,
                    alpha = 1, beta = 1) %>%
  stop_at_n(n = 30)
sc1_ <- simulate_trials(
  design_,
  num_sims = 5000,
  true_prob_tox = true_prob_tox
)
sc1_


# Sc2 of Guo et al ----
true_prob_tox = c(0.04, 0.06, 0.08, 0.10, 0.3, 0.6)
mtd <- 5

file_loc <- "Scratch/mTPI2/GuoScenario2.rds"
if(file.exists(file_loc)) {
  sims <- readRDS(file = file_loc)
} else {
  set.seed(2023)
  sims <- simulate_trials(
    design,
    num_sims = 10^4,
    true_prob_tox = true_prob_tox
  )
  saveRDS(sims, file = file_loc)
}
pr <- prob_recommend(sims)
pa <- prob_administer(sims)

mtd_equiv_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox >= target - epsilon1 & true_prob_tox <= target + epsilon2
]
mtd_dose_names <- unique(c(mtd_equiv_dose_names, as.character(mtd)))
safe_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox <= target + epsilon2
]
mtd_dose_names %>% map_dbl(~ pr[[.x]]) %>% sum()
safe_dose_names %>% map_dbl(~ pa[[.x]]) %>% sum()


# Sc3 of Guo et al ----
true_prob_tox = c(0.01, 0.05, 0.10, 0.60, 0.70, 0.90)
mtd <- 3

file_loc <- "Scratch/mTPI2/GuoScenario3.rds"
if(file.exists(file_loc)) {
  sims <- readRDS(file = file_loc)
} else {
  set.seed(2023)
  sims <- simulate_trials(
    design,
    num_sims = 10^4,
    true_prob_tox = true_prob_tox
  )
  saveRDS(sims, file = file_loc)
}
pr <- prob_recommend(sims)
pa <- prob_administer(sims)

mtd_equiv_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox >= target - epsilon1 & true_prob_tox <= target + epsilon2
]
mtd_dose_names <- unique(c(mtd_equiv_dose_names, as.character(mtd)))
safe_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox <= target + epsilon2
]
mtd_dose_names %>% map_dbl(~ pr[[.x]]) %>% sum()
safe_dose_names %>% map_dbl(~ pa[[.x]]) %>% sum()

# Sc4 of Guo et al ----
true_prob_tox = c(0.25, 0.27, 0.29, 0.31, 0.33, 0.35)
mtd <- 3

file_loc <- "Scratch/mTPI2/GuoScenario4.rds"
if(file.exists(file_loc)) {
  sims <- readRDS(file = file_loc)
} else {
  set.seed(2023)
  sims <- simulate_trials(
    design,
    num_sims = 10^4,
    true_prob_tox = true_prob_tox
  )
  saveRDS(sims, file = file_loc)
}
pr <- prob_recommend(sims)
pa <- prob_administer(sims)

mtd_equiv_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox >= target - epsilon1 & true_prob_tox <= target + epsilon2
]
mtd_dose_names <- unique(c(mtd_equiv_dose_names, as.character(mtd)))
safe_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox <= target + epsilon2
]
mtd_dose_names %>% map_dbl(~ pr[[.x]]) %>% sum()
safe_dose_names %>% map_dbl(~ pa[[.x]]) %>% sum()


# Sc5 of Guo et al ----
true_prob_tox = c(0.35, 0.45, 0.50, 0.60, 0.70, 0.80)
mtd <- "1"

file_loc <- "Scratch/mTPI2/GuoScenario5.rds"
if(file.exists(file_loc)) {
  sims <- readRDS(file = file_loc)
} else {
  set.seed(2023)
  sims <- simulate_trials(
    design,
    num_sims = 10^4,
    true_prob_tox = true_prob_tox
  )
  saveRDS(sims, file = file_loc)
}
pr <- prob_recommend(sims)
pa <- prob_administer(sims)

mtd_equiv_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox >= target - epsilon1 & true_prob_tox <= target + epsilon2
]
mtd_dose_names <- unique(c(mtd_equiv_dose_names, as.character(mtd)))
safe_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox <= target + epsilon2
]
mtd_dose_names %>% map_dbl(~ pr[[.x]]) %>% sum()
pr["NoDose"]
safe_dose_names %>% map_dbl(~ pa[[.x]]) %>% sum()

# Sc6 of Guo et al ----
true_prob_tox = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
mtd <- "NoDose"

file_loc <- "Scratch/mTPI2/GuoScenario6.rds"
if(file.exists(file_loc)) {
  sims <- readRDS(file = file_loc)
} else {
  set.seed(2023)
  sims <- simulate_trials(
    design,
    num_sims = 10^4,
    true_prob_tox = true_prob_tox
  )
  saveRDS(sims, file = file_loc)
}
pr <- prob_recommend(sims)
pa <- prob_administer(sims)

mtd_equiv_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox >= target - epsilon1 & true_prob_tox <= target + epsilon2
]
mtd_dose_names <- unique(c(mtd_equiv_dose_names, as.character(mtd)))
safe_dose_names <- as.character(dose_indices(sims))[
  true_prob_tox <= target + epsilon2
]
mtd_dose_names %>% map_dbl(~ pr[[.x]]) %>% sum()
pr["NoDose"]
safe_dose_names %>% map_dbl(~ pa[[.x]]) %>% sum()


# Check iso regression ----
path = "1NNN 2TNNNNN 3NNN 4NNNNNN 5NNN 6TTNNNN"
model_fitter <-
  get_mtpi2(num_doses = 6, target = 0.3,
            epsilon1 = 0.05, epsilon2 = 0.05,
            exclusion_certainty = 0.95,
            alpha = 1, beta = 1) %>%
  # follow_path(path = path) %>%
  stop_at_n(n = 27) %>%
  select_mtpi2_mtd(
    when = 'finally', target = 0.3,
    exclusion_certainty = 0.95,
    # alpha = 0.001, beta = 0.001
    alpha = 1, beta = 1
  )
x
# Fix when alpha and (or?) beta = 0
x <- model_fitter %>% fit(path)
continue(x)
recommended_dose(x) # TODO fix me
mean_prob_tox(x)
x %>% mean_prob_tox() # Breaks
x$parent %>% mean_prob_tox() # OK
x$parent$parent %>% mean_prob_tox() # OK

pava <- function(x, wt = rep(1, length(x))) {
  n <- length(x)
  if (n <= 1)
    return(x)
  if (any(is.na(x)) || any(is.na(wt))) {
    stop("Missing values in 'x' or 'wt' is not allowed")
  }
  lvlsets <- 1:n
  repeat {
    viol <- as.vector(diff(x)) < 0
    if(!(any(viol)))
      return(x)

    i <- min((1:(n - 1))[viol])
    lvl1 <- lvlsets[i]
    lvl2 <- lvlsets[i + 1]
    ilvl <- (lvlsets == lvl1 | lvlsets == lvl2)
    x[ilvl] <- sum(x[ilvl] * wt[ilvl])/sum(wt[ilvl])
    lvlsets[ilvl] <- lvl1
  }
  x
}
# mean_prob_tox.mtpi2_mtd_dose_selector
post_mean = (x$alpha + tox_at_dose(x)) / (x$alpha + x$beta + n_at_dose(x))
post_alpha = x$alpha + tox_at_dose(x)
post_beta = x$beta + n_at_dose(x) - tox_at_dose(x)
post_var = ((post_alpha) * (post_beta)) /
  ( (post_alpha + post_beta)^2 * (post_alpha + post_beta + 1) )
round(post_var, 3)
post_mean = pava(post_mean, wt = 1 / post_var)
x = post_mean
wt = 1 / post_var

# Reproduce Shiny iso.est
post_alpha = 0 + tox_at_dose(x)
post_beta = 0 + n_at_dose(x) - tox_at_dose(x)
post_mean = (post_alpha) / (post_alpha + post_beta)
post_var = ((post_alpha) * (post_beta)) /
  ( (post_alpha + post_beta)^2 * (post_alpha + post_beta + 1) )
post_var[post_var == 0] <- 0.03#max(post_var)
round(post_var, 3)
xyz = pava(post_mean, wt = 1 / post_var)
xyz
# The AZ app:
# 1) does not use the prior alpha & beta when calculating prob_tox for pava
# 2) implements a manual fix to avoid zero variance (suspect using max(var)),
#    a prob that exists because alpha and beta not used.

pava(c(0, 1/6, 0, 0, 0, 1/3))
pava(c(0, 1/6, 0, 0, 0, 1/3), wt = c())
