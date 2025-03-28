
# knitr::purl("~/OneDrive - AZCollaboration/Documents/2 - Areas/
# stats_innov_site/content/post/boin-comb/index.Rmd")


library(escalation)

## Single treatment dose-finding ----

# Model
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
model <- get_dfcrm(skeleton = skeleton, target = target)


# Outcomes
parse_phase1_outcomes("2NNN 3NTN", as_list = FALSE)


## Fit
fit <- model %>% fit("2NNN 3NTN")
fit


## Should we continue? At which dose?
continue(fit)
recommended_dose(fit)


## More outcomes
fit <- model %>% fit("2NNN 3NTN 3NNT")

## Should we continue? At which dose?
continue(fit)
recommended_dose(fit)


## We can tailor behaviour by bolting on classes

# Stop at total sample size
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 9)

fit <- model %>% fit("2NNN 3NTN 3NNT")

continue(fit)
recommended_dose(fit)


# Stop at sample size at a particular dose
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 6, dose = "recommended")

fit <- model %>% fit("2NNN 3NTN 3NNT")

continue(fit)
recommended_dose(fit)


# Combine both!
model <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 30) %>%
  stop_when_n_at_dose(n = 12, dose = "recommended")

fit <- model %>% fit("2NNN 3NTN 3NNT")

continue(fit)
recommended_dose(fit)


## Treatment combination dose-finding ----
# The number of doses is now a vector
num_doses <- c(3, 4)

# Model
target <- 0.25
model <- get_boin_comb(num_doses = num_doses, target = target)


## Outcome syntax needs some tweaks
outcomes <- "2.1NNN 3.1TTT"
parse_phase1_outcomes(outcomes, as_list = FALSE)

library(tidyverse)
parse_phase1_outcomes(outcomes, as_list = FALSE) %>%
  mutate(
    d1 = map_int(dose, ~ .x[1]),
    d2 = map_int(dose, ~ .x[2])
  )

# Fit
fit <- model %>% fit(outcomes = outcomes)
fit


## Should we continue? At which dose?
continue(fit)
recommended_dose(fit)


# We can extract lots of useful info
n_at_dose(fit)
tox_at_dose(fit)
# Observed tox rates
empiric_tox_rate(fit)
# Modelled tox rates using PAVA isotonic regression
mean_prob_tox(fit)
dose_admissible(fit)


# We can tailor behaviour using the same classes
model <- get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 30) %>%
  stop_when_n_at_dose(n = 12, dose = "recommended")

fit <- model %>% fit(outcomes = outcomes)
continue(fit)
recommended_dose(fit)

# Please see:
# https://rstudio-connect.seml.scp.astrazeneca.net/stats-innov/post/boin-comb/

