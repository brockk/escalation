
library(escalation)

# Parsing ----
outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
phase1_outcomes_to_cohorts(outcomes)
parse_phase1_outcomes(outcomes)
parse_phase1_outcomes(outcomes, as_list = FALSE)


# dfcrm ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

## Classic R
crm_fitter <- get_dfcrm(skeleton = skeleton, target = target)
# Factory interface
outcomes <- '1NNN 2NNN 3NNT'
x <- fit(crm_fitter, outcomes)
x

# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_tox_quantile(x, p = 0.95)
#  and standard generics
print(x)
summary(x)
as_tibble(x)

# Using tibble
outcomes <- tibble::tibble(
  cohort = c(1,1, 2,2, 3,3),
  dose = c(1,1, 2,2, 3,3),
  tox = c(0,0, 0,0, 1,1)
)
x <- fit(crm_fitter, outcomes)
# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, 0.5)

## logit model
crm_fitter <- get_dfcrm(skeleton = skeleton, target = target, intcpt = 4,
                        model = 'logistic')
x <- crm_fitter %>% fit(outcomes)
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_tox_quantile(x, p = 0.95)
#  and standard generics
print(x)
summary(x)

## tidyverse R
show_examples <- function(crm_fit) {
  cat('class:', crm_fit %>% class(), '\n')
  cat('num_patients:', crm_fit %>% num_patients(), '\n')
  cat('cohort:', crm_fit %>% cohort(), '\n')
  cat('doses_given:', crm_fit %>% doses_given(), '\n')
  cat('tox:', crm_fit %>% tox(), '\n')
  # print('model_frame:', crm_fit %>% model_frame(), '\n')
  cat('num_doses:', crm_fit %>% num_doses(), '\n')
  cat('recommended_dose:', crm_fit %>% recommended_dose(), '\n')
  cat('continue:', crm_fit %>% continue(), '\n')
  cat('n_at_dose:', crm_fit %>% n_at_dose(), '\n')
  cat('tox_at_dose:', crm_fit %>% tox_at_dose(), '\n')
  cat('empiric_tox_rate:', crm_fit %>% empiric_tox_rate(), '\n')
  cat('mean_prob_tox:', crm_fit %>% mean_prob_tox(), '\n')
  cat('median_prob_tox:', crm_fit %>% median_prob_tox(), '\n')
  cat('prob_tox_exceeds_0.5:', crm_fit %>% prob_tox_exceeds(0.5), '\n')
}

library(magrittr)
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  fit(outcomes = '') %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 6, dose = 2) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 15) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 12, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.5, confidence = 0.7) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  stop_at_n(n = 21) %>%
  stop_when_too_toxic(dose = 5, tox_threshold = 0.5, confidence = 0.7) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(dose = 5, tox_threshold = 0.5, confidence = 0.7) %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 9, dose = 2) %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(tox_threshold = 0.5, confidence = 0.9, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 12, dose = 'any') %>%
  fit(outcomes) %>%
  show_examples()
crm_fit <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_when_too_toxic(tox_threshold = 0.5, confidence = 0.75, dose = 'any') %>%
  stop_at_n(n = 21) %>%
  stop_when_n_at_dose(n = 12, dose = 'any') %>%
  fit(outcomes) %>%
  show_examples()


# BOIN ----
num_doses <- 5
target <- 0.3

boin_fitter <- get_boin(num_doses = num_doses, target = target)
x <- fit(boin_fitter, '1NNN')
x

class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, target)
x$boin_fit$p_overdose

# Using tibble
outcomes <- data.frame(
  cohort = c(1,1, 2,2, 3,3),
  dose = c(1,1, 2,2, 3,3),
  tox = c(0,0, 0,0, 1,1)
)
x <- fit(boin_fitter, outcomes)
# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, target)
x$boin_fit$p_overdose
prob_tox_exceeds(x, target + 0.1)

# Figure 10 of Yan, Pan, Zhang, Liu & Yuan
num_doses <- 5
target <- 0.3
boin_fitter <- get_boin(num_doses = num_doses, target = target)

x <- fit(boin_fitter, '1NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT')
recommended_dose(x)

x <- fit(boin_fitter, '1NNN 2NNN 3NTT 2NTN 3NNN 3NNN 4TTT 3NTN 3NNT 3TNN')
recommended_dose(x)

n_at_dose(x)
tox_at_dose(x)


# 3+3 ----
three_plus_three_fitter <- get_three_plus_three(num_doses = 5)
x <- three_plus_three_fitter %>% fit('1NNN 2NTT')
x

class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, 0.5)
tox_target(x)


# Using tibble
outcomes <- data.frame(
  cohort = c(1,1,1, 2,2,2),
  dose = c(1,1,1, 2,2,2),
  tox = c(0,0, 0,0, 1,1)
)
three_plus_three(outcomes = outcomes, num_doses = num_doses)
x <- fit(three_plus_three_fitter, outcomes)
# Selector interface
class(x)
num_patients(x)
cohort(x)
doses_given(x)
tox(x)
model_frame(x)
num_doses(x)
recommended_dose(x)
continue(x)
n_at_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
prob_tox_exceeds(x, 0.5)




# Dose paths ----

# Use 3+3
selector_factory <- get_three_plus_three(num_doses = 5)
cohort_sizes <- c(3, 3, 3)

# Use CRM
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
selector_factory <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12)
selector_factory <- get_dfcrm(skeleton = skeleton, target = target) %>%
  dont_skip_doses() %>%
  stop_when_n_at_dose(dose = 'recommended', n = 9)

# Use BOIN
selector_factory <- get_boin(num_doses = length(skeleton), target = target) %>%
  stop_at_n(n = 12)

# Get paths
paths1 <- selector_factory %>% get_dose_paths(cohort_sizes = cohort_sizes)
as_tibble(paths1) %>% print(n = 100)

spread_paths(as_tibble(paths1)) %>%
  select('outcomes0', 'next_dose0', 'outcomes1', 'next_dose1',
         'outcomes2', 'next_dose2', 'outcomes3', 'next_dose3') %>%
  print(n=100)
# With true_prob_tox, these paths have exact probabilities

# In-progress trials:
paths2 <- selector_factory %>%
  get_dose_paths(cohort_sizes = cohort_sizes, previous_outcomes = '1NTN')
as_tibble(paths2) %>% print(n = 100)
spread_paths(as_tibble(paths2)) %>%
  select('outcomes0', 'next_dose0', 'outcomes1', 'next_dose1',
         'outcomes2', 'next_dose2', 'outcomes3', 'next_dose3') %>%
    print(n=100)

paths3 <- selector_factory %>%
  get_dose_paths(cohort_sizes = cohort_sizes, previous_outcomes = '1NTT')
as_tibble(paths3) %>% print(n = 100)
spread_paths(as_tibble(paths3)) %>%
  select('outcomes0', 'next_dose0', 'outcomes1', 'next_dose1',
         'outcomes2', 'next_dose2', 'outcomes3', 'next_dose3') %>%
  print(n = 100)

paths <- paths1
class(paths) # dose_paths
length(paths) # num_paths
class(paths[[1]]) # dose_finding_path_node

paths
print(paths, node = 10)
print(paths, node = 90)
print(paths, node = -1)
print(paths, node = NA)
print(paths, node = NULL)



# Visualise
df <- as_tibble(paths)
spread_paths(df) # wide
spread_paths(df %>% select(-fit, -parent_fit, -dose_index)) # briefer
library(DiagrammeR)
library(dplyr)

library(RColorBrewer)
display.brewer.all()
dose_indices(paths)

graph_paths <- function(paths, palette = 'Spectral') {
  stop_label <- 'Stop'
  df <- as_tibble(paths)
  df_colour <- tibble(
    dose = c(stop_label, as.character(dose_indices(x))),
    fillcolor = RColorBrewer::brewer.pal(num_doses(x) + 1, palette)
  )
  df %>%
    transmute(id = .node,
              type = NA,
              next_dose,
              label = case_when(
                is.na(next_dose) ~ 'Stop',
                TRUE ~ next_dose %>% as.character())
    ) %>%
    left_join(df_colour, by = c('label' = 'dose')) -> ndf

  df %>%
    filter(!is.na(.parent)) %>%
    select(from = .parent, to = .node, label = outcomes) %>%
    mutate(rel = "leading_to") -> edf

  graph <- create_graph(nodes_df = ndf, edges_df = edf)
  render_graph(graph)
}
graph_paths(paths)
graph_paths(paths, pal = 'Blues')
graph_paths(paths, pal = 'Paired')
graph_paths(paths, pal = 'RdPu')
graph_paths(paths, pal = 'PuRd')
graph_paths(paths, pal = 'PRGn')

# We could append this graph with transition probabilities.


# Crytallise
cohort_sizes <- c(3, 3, 3)
paths <- get_three_plus_three(num_doses = 5) %>%
  get_dose_paths(cohort_sizes = cohort_sizes)

class(paths)
num_doses(paths)

true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
x <- calculate_probabilities(paths, true_prob_tox)

prob_recommend(x)
prob_administer(x)


# Simulation ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

# Sc 1
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox) -> sims
sims

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate_trials(num_sims = 5, true_prob_tox = true_prob_tox,
           return_all_fits = TRUE) -> sims

get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

get_boin(num_doses = length(skeleton), target = target) %>%
  stop_at_n(n = 12) %>% simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

class(sims) # simulations
length(sims) # Num sims
class(sims[[1]]) # list
length(sims[[1]]) # Num decisions
class(sims[[1]][[1]]) # list
names(sims[[1]][[1]]) # "cohort" "time"   "fit"
class(sims[[1]][[1]]$fit) # selector


# Interface
class(sims)
length(sims)
summary(num_patients(sims))
num_doses(sims)
dose_indices(sims)
n_at_dose(sims) %>% colMeans()
num_tox(sims) %>% mean
tox_at_dose(sims) %>% colMeans()
object.size(sims) %>% format(units = 'MB')
recommended_dose(sims)
prob_recommend(sims)
prob_administer(sims)
trial_duration(sims)
summary(trial_duration(sims))
as_tibble(sims) %>% print(n = 30)

library(ggplot2)
as_tibble(sims) %>%
  ggplot(aes(x = dose, y = mean_prob_tox)) +
  geom_line(aes(group = .iteration))

as_tibble(sims) %>%
  ggplot(aes(x = dose, y = mean_prob_tox)) +
  geom_line(aes(group = .iteration)) +
  facet_wrap(~ .iteration)

# ThreePlusThree in bcrm calculates exact OCs.
bcrm_3p3 <- bcrm::threep3(
  truep = true_prob_tox,
  threep3.start = 1, threep3.esc.only = TRUE)
bcrm_3p3



# Sc 2
true_prob_tox <- c(0.03, 0.09, 0.16, 0.27, 0.42)

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate_trials(num_sims = 500, true_prob_tox = true_prob_tox) -> threeps
crm_fitter %>% simulate_trials(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function(current_data)
    cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> crm_sims
boin_fitter %>% simulate_trials(
  num_sims = 500,
  true_prob_tox = true_prob_tox,
  sample_patient_arrivals = function(current_data)
    cohorts_of_n(n = 2, mean_time_delta = 1),
  next_dose = 2) -> boin_sims

prob_recommend(threeps)
prob_recommend(crm_sims)
prob_recommend(boin_sims)

dplyr::bind_rows(
  tibble(Method = '3+3', Dose = 0:5, ProbSelect = prob_recommend(threeps)),
  tibble(Method = 'CRM', Dose = 0:5, ProbSelect = prob_recommend(crm_sims)),
  tibble(Method = 'BOIN', Dose = 0:5, ProbSelect = prob_recommend(boin_sims))
) %>% ggplot(aes(x = Dose, y = ProbSelect, col = Method)) +
  geom_point() + geom_line() +
  labs(title = 'Model-based vs rule-based dose-finding',
    subtitle = 'Model-based methods allocate more patients close to target dose.')

# ThreePlusThree in bcrm
bcrm_3p3 <- bcrm::threep3(
  truep = true_prob_tox,
  threep3.start = 1, threep3.esc.only = TRUE)
bcrm_3p3



# Help files ----

# Interface
? selector
? selector_factory
? fit
? num_patients
? cohort
? doses_given
? tox
? num_tox
? model_frame
? num_doses
? recommended_dose
? continue
? n_at_dose
? prob_administer
? tox_at_dose
? empiric_tox_rate
? mean_prob_tox
? median_prob_tox
? prob_tox_quantile
? prob_tox_exceeds

# selector_factorys
? get_dfcrm
? get_boin
? get_three_plus_three
? dont_skip_doses
? stop_when_n_at_dose
? stop_at_n
? stop_when_too_toxic
? demand_n_at_dose
? follow_path

