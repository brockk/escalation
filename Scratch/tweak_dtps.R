
library(escalation)

# Phase 1 design
model <- get_dfcrm(
  skeleton = dfcrm::getprior(0.1, 0.25, nu = 3, nlevel = 5),
  target = 0.25
) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.25, confidence = 0.9)

x1 <- model %>% fit("1NNN 2TTT 1TTT")
x1

abc1 <- model %>% get_dose_paths(
  cohort_sizes = c(3),
  previous_outcomes = "1NNN 2TTT 1TTT",
  next_dose = 1
)
abc1
length(abc1)

# c.f.
abc2 <- model %>% get_dose_paths(
  cohort_sizes = c(3),
  previous_outcomes = "1NNN 2TTT 1TTT"
)
abc2
length(abc2)



# Phase 1/2 design
model <- get_boin12(num_doses = 5, phi_t = 0.35, phi_e = 0.25,
                    u2 = 40, u3 = 60, n_star = 6) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.25, confidence = 0.9)

x1 <- model %>% fit("1NNN 2TTT 1TTT")
x1

abc1 <- model %>% get_dose_paths(
  cohort_sizes = c(3),
  previous_outcomes = "1NNN 2TTT 1TTT",
  next_dose = 1
)
abc1
length(abc1)

# c.f.
abc2 <- model %>% get_dose_paths(
  cohort_sizes = c(3),
  previous_outcomes = "1NNN 2TTT 1TTT"
)
abc2
length(abc2)

# Both phase 1 and phase 1/2 dose-paths algos will continue if previous_outcomes
# and next_dose are specified, even if the design fit to previous_outcomes would
# normally advocate stopping. This allows useful exploration.
