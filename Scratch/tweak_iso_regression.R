
library(dplyr)
library(escalation)

# Dan's example
mtpi2_mod <- get_mtpi2(
  num_doses = 5,
  target = 0.3,
  alpha = 1,
  beta = 1,
  epsilon1 = 0.5,
  epsilon2 = 0.5,
  exclusion_certainty = 0.5,
  stop_when_deescalation_impossible = TRUE
) %>%
  unadmit_untested() %>%
  select_mtpi2_mtd(when = "always", exclusion_certainty = 0.9,
                   alpha = 1,
                   beta = 1,
                   target = 0.3,
                   pava_just_tested_doses = TRUE)
mtpi2_mod$pava_just_tested_doses

x = mtpi2_mod %>% fit("2TTTTTT")
x
x = mtpi2_mod %>% fit("1NNNNNNNNNN 4T")
x
summary(x)
