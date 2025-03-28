
library(escalation)

skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
model <- get_dfcrm_tite(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 9)

set.seed(2025)
sims1 <- simulate_trials(
  model,
  num_sims = 10,
  true_prob_tox = seq_len(5) / 10,
  max_time = 10,
  next_dose = 3
)


# Comb
num_doses <- c(3, 4)
target <- 0.25
model <- get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 12)
true_prob_tox <- matrix(c(0.1, 0.2, 0.3, 0.4,
                          0.25, 0.35, 0.45, 0.55,
                          0.3, 0.4, 0.5, 0.6),
                        nrow = num_doses[1],
                        ncol = num_doses[2],
                        byrow = TRUE)
sims1 <- model %>%
  simulate_trials(num_sims = 10, true_prob_tox = true_prob_tox)

num_patients(sims1) # OK
num_doses(sims1) # OK
dose_indices(sims1) # OK
dose_strings(sims1) # OK
doses_given(sims1, dose_strings = TRUE) # OK
doses_given(sims1, dose_strings = FALSE) # OK
recommended_dose(sims1, dose_string = TRUE) # OK
recommended_dose(sims1, dose_string = FALSE) # OK
n_at_dose(sims1) # OK
n_at_dose(sims1, dose = "recommended") # OK
n_at_recommended_dose(sims1) # OK
tox_at_dose(sims1) # OK
num_tox(sims1) # OK
eff_at_dose(sims1) # OK
num_eff(sims1) # OK
prob_recommend(sims1) # OK
prob_administer(sims1, method = 0) # OK
prob_administer(sims1, method = 1) # OK
trial_duration(sims1) # OK
print(sims1) # OK
summary(sims1) # OK
as_tibble(sims1) # OK


myfit <- sims1$fits %>% .[[2]] %>% .[[1]] %>% .$fit
myfit$parent$df # dose is wrong
fit2 = model %>% fit(outcomes = myfit$parent$df)
fit2$parent$df
selector_factory = model
outcomes = new_data
df = new_data

x = myfit$parent
class(myfit)
class(myfit$parent)
# TODO dose in simulated object is integer of length 1. Wrong
doses_given(myfit)
doses_given(myfit$parent)
df = parse_phase1_outcomes("1.1NNN 1.2NTN", as_list = FALSE)
source("R/helpers.R")
spruce_outcomes_df(df)

# Create doses_given for simulations
sims1$fits %>%
  map(~ tail(.x, 1)[[1]]) %>%
  map("fit") %>%
  map(doses_given) %>%
  reduce(rbind) %>%
  unname()

# doses_given is broken for boin_comb_selector

# TODO Add TITE CRM tests
