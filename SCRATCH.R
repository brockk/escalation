
library(escalation)
library(tidyr)

model <- get_dfcrm(
  skeleton = dfcrm::getprior(0.05, 0.25, nu = 3, nlevel = 5, model = 'logistic'),
  target = 0.25, model = 'logistic',
  intcpt = 3, scale = sqrt(1)
) %>%
  stop_at_n(n = 30) %>%
  stop_when_n_at_dose(n = 12, dose = 'recommended') %>%
  dont_skip_doses() %>%
  stop_when_too_toxic(dose = 1, tox_threshold = 0.25, confidence = 0.8)

x <- model %>% fit("1NNN 2NTT 2TTT")
class(x)
recommended_dose(x)
continue(x)
dose_admissible(x)

print(x) # OK
as_tibble(x) # OK
summary(x) # OK


model <- get_dfcrm(
  skeleton = dfcrm::getprior(0.05, 0.25, nu = 3, nlevel = 5, model = 'logistic'),
  target = 0.25, model = 'logistic',
  intcpt = 3, scale = sqrt(1)
)

x <- model %>% fit("1NNN 2NTT 2TTT")
class(x)
recommended_dose(x)
continue(x)
dose_admissible(x)

print(x) # OK
as_tibble(x) # OK
summary(x) # OK
