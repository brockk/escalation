
? get_dfcrm

skeleton <- c(0.05, 0.1, 0.25, 0.4, 0.6)
target <- 0.33
model1 <- get_dfcrm(skeleton = skeleton, target = target)

# By default, dfcrm fits the empiric model:
outcomes <- '1NTN'
fit1 <- model1 %>% fit(outcomes)
fit1 %>% recommended_dose()
fit1 %>% mean_prob_tox()
abs(fit1 %>% mean_prob_tox() - target)

library(dplyr)
prob_tox <- fit1 %>% prob_tox_samples() %>% select(-.draw)
a <- 0.5
num <- (prob_tox - target)^2
denom <- (prob_tox^a) * (1 - prob_tox)^(2 - a)
colMeans(num / denom)
which.min(colMeans(num / denom)) %>% unname()
# Different dose to Euclidean distance minimiser.
