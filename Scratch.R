
selector <- get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25)
x <- fit(selector, '1NNN 2NTN')
recommended_dose(x)

library(magrittr)
model <- get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25) %>%
  fit('1NNN 2NTN')
model %>% recommended_dose()
model %>% n_at_dose()

get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25) %>%
  stop_when_n_at_dose(n = 3, dose = 2) %>%
  fit('1NNN 2NTN') %>% recommended_dose()
get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25) %>%
  stop_when_n_at_dose(n = 3, dose = -1) %>%
  fit('1NNN 2NTN') %>% recommended_dose()
get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25) %>%
  stop_when_n_at_dose(n = 3, dose = 0) %>%
  fit('1NNN 2NTN') %>% recommended_dose()
get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25) %>%
  stop_when_n_at_dose(n = 3, dose = 6) %>%
  fit('1NNN 2NTN') %>% recommended_dose()
get_dfcrm(skeleton = c(0.05, 0.1, 0.25, 0.4, 0.6), target = 0.25) %>%
  stop_when_n_at_dose(n = 4, dose = 2) %>%
  fit('1NNN 2NTN') %>% recommended_dose()
# Cascade works. Looking good.
