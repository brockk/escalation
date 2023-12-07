
target <- 0.3
model <- get_mtpi2(num_doses = 6, target = target, epsilon1 = 0.05,
                   epsilon2 = 0.05, exclusion_certainty = 0.95,
                   alpha = 1, beta = 1) %>%
  stop_when_n_at_dose(n = 12, dose = "any") %>%
  select_mtpi2_mtd(when = "finally", exclusion_certainty = 0.95) %>%
  stop_at_n(n = 27)

x <- model %>% fit("1NNN 2NNNNNT 3NNT 4NNNNNN 5NNT 6NNNTTT")
num_patients(x)
mean_prob_tox(x)
continue(x)
