
library(escalation)

skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25


# Parsing ----
outcomes <- '1NNN 2NNN 3NNT 3NNN 3TNT 2NNN'
phase1_outcomes_to_cohorts(outcomes)
parse_phase1_outcomes(outcomes)
parse_phase1_outcomes(outcomes, as_list = FALSE)


# dfcrm ----

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
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_tox_quantile(x, p = 0.95)
# and standard generics
print(x)
summary(x)
as_tibble(x)


# trialr ----

# Empiric
crm_fitter <- get_trialr_crm(skeleton = skeleton, target = target,
                             model = 'empiric', beta_sd = 1.34)
# 1-param logistic
crm_fitter <- get_trialr_crm(skeleton = skeleton, target = target,
                             model = 'logistic', a0 = 3,
                             beta_mean = 0, beta_sd = 1.34)
# 2-param logistic
crm_fitter <- get_trialr_crm(skeleton = skeleton, target = target,
                             model = 'logistic2',
                             alpha_mean = 0, alpha_sd = 2,
                             beta_mean = 0, beta_sd = 1.34)

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
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
prob_tox_quantile(x, p = 0.05)
prob_tox_quantile(x, p = 0.5)
prob_tox_quantile(x, p = 0.95)
# and standard generics
print(x)
summary(x)
as_tibble(x)



# BOIN ----
num_doses <- 5
target <- 0.3

boin_fitter <- get_boin(num_doses = num_doses, target = target)
x <- fit(boin_fitter, '1NNN')
x <- fit(boin_fitter, '1NNN 2TTNT 3NNT')
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
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)
x$boin_fit$p_overdose


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


# TPI example ----
tpi_fitter <- get_tpi(num_doses = 5, target = 0.3, k1 = 1, k2 = 1.5,
                      exclusion_certainty = 0.7)
x <- fit(tpi_fitter, '1NNN')
x <- fit(tpi_fitter, '1NNN 2TTNT')
# x <- fit(tpi_fitter, '1NNN 2TTNT 3NNT')
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
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, target)

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
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
empiric_tox_rate(x)
mean_prob_tox(x)
median_prob_tox(x)
dose_admissible(x)
prob_tox_exceeds(x, 0.5)
tox_target(x)


# Dose paths ----
cohort_sizes <- c(3, 3, 3)

# Use 3+3
selector_factory <- get_three_plus_three(num_doses = 5)

# Use dfcrm
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
selector_factory <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12)
selector_factory <- get_dfcrm(skeleton = skeleton, target = target) %>%
  dont_skip_doses() %>%
  stop_when_n_at_dose(dose = 'recommended', n = 9)

# Use trialr CRM
# 2-param logistic
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
selector_factory <- get_trialr_crm(skeleton = skeleton, target = target,
                                   model = 'logistic2',
                                   alpha_mean = 0, alpha_sd = 2,
                                   beta_mean = 0, beta_sd = 1.34)


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

# graph_paths <- function(paths,
#                         viridis_palette = 'viridis',
#                         RColorBrewer_palette = NULL
#                         ) {
#
#   stop_label <- 'Stop'
#   df <- as_tibble(paths)
#
#   num_colours <- num_doses(x) + 1
#
#   if(is.null(RColorBrewer_palette)) {
#     df_colour <- tibble(
#       dose = c(stop_label, as.character(dose_indices(x))),
#       fillcolor = viridis::viridis(num_colours, option = viridis_palette)
#     )
#   } else {
#     df_colour <- tibble(
#       dose = c(stop_label, as.character(dose_indices(x))),
#       fillcolor = RColorBrewer::brewer.pal(num_colours, RColorBrewer_palette)
#     )
#   }
#
#   col_offset <- as.integer(num_colours / 2)
#   i <- 1 + mod((seq_along(df_colour$fillcolor) + col_offset - 1), num_colours)
#   df_colour$fontcolor <- df_colour$fillcolor[i]
#
#   df %>%
#     transmute(id = .node,
#               type = NA,
#               next_dose,
#               label = case_when(
#                 is.na(next_dose) ~ 'Stop',
#                 TRUE ~ next_dose %>% as.character())
#     ) %>%
#     left_join(df_colour, by = c('label' = 'dose')) -> ndf
#
#   df %>%
#     filter(!is.na(.parent)) %>%
#     select(from = .parent, to = .node, label = outcomes) %>%
#     mutate(rel = "leading_to") -> edf
#
#   graph <- DiagrammeR::create_graph(nodes_df = ndf, edges_df = edf)
#   DiagrammeR::render_graph(graph)
# }

graph_paths(paths)

graph_paths(paths, viridis_palette = 'viridis')
graph_paths(paths, viridis_palette = 'magma')
graph_paths(paths, viridis_palette = 'plasma')
graph_paths(paths, viridis_palette = 'inferno')
graph_paths(paths, viridis_palette = 'cividis')
# Or
graph_paths(paths, RColorBrewer_palette = 'YlOrRd')
graph_paths(paths, RColorBrewer_palette = 'Blues')
graph_paths(paths, RColorBrewer_palette = 'Paired')
graph_paths(paths, RColorBrewer_palette = 'RdPu')
graph_paths(paths, RColorBrewer_palette = 'Set2')
graph_paths(paths, RColorBrewer_palette = 'Spectral')



# We could append this graph with transition probabilities.

# Crytallised dose-paths ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

cohort_sizes <- c(3, 3, 5)
paths <- get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  get_dose_paths(cohort_sizes = cohort_sizes)

class(paths)
num_doses(paths)

true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)
x <- calculate_probabilities(paths, true_prob_tox)
x

summary(x)
num_patients(x)
num_doses(x)
dose_indices(x)
n_at_dose(x)
sum(n_at_dose(x))
n_at_dose(x, dose = 0)
n_at_dose(x, dose = 1)
n_at_dose(x, dose = 'recommended')
n_at_recommended_dose(x)
tox_at_dose(x)
sum(tox_at_dose(x))
num_tox(x)
prob_recommend(x)
prob_administer(x)


# Simulation ----
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25

# Sc 1
true_prob_tox <- c(0.12, 0.27, 0.44, 0.53, 0.57)

# Use 3+3
get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate_trials(num_sims = 50, true_prob_tox = true_prob_tox) -> sims
sims

get_three_plus_three(num_doses = length(skeleton)) %>%
  simulate_trials(num_sims = 5, true_prob_tox = true_prob_tox,
           return_all_fits = TRUE) -> sims

# Use dfcrm
get_dfcrm(skeleton = skeleton, target = target) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

# Use trialr CRM
skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.25
get_trialr_crm(skeleton = skeleton, target = target,
               model = 'logistic2',
               alpha_mean = 0, alpha_sd = 2,
               beta_mean = 0, beta_sd = 1.34) %>%
  stop_at_n(n = 12) %>%
  simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

# Use BOIN
get_boin(num_doses = length(skeleton), target = target) %>%
  stop_at_n(n = 12) %>% simulate_trials(
    num_sims = 50,
    true_prob_tox = true_prob_tox,
    sample_patient_arrivals =
      function(current_data) cohorts_of_n(n = 2, mean_time_delta = 1),
    next_dose = 2) -> sims

length(sims) # Num sims
class(sims[[1]]) # list
length(sims[[1]]) # Num decisions
class(sims[[1]][[1]]) # list


# Interface
class(sims)
length(sims)
summary(sims)
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
tibble::as_tibble(sims) %>% print(n = 30)

library(ggplot2)
tibble::as_tibble(sims) %>%
  ggplot(aes(x = dose, y = mean_prob_tox)) +
  geom_line(aes(group = .iteration))

tibble::as_tibble(sims) %>%
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
? dose_admissible
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

