library(bcrm)

# To check empiric stan_crm fit ----
dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
skeleton <- c(0.010, 0.015, 0.020, 0.025, 0.030, 0.040, 0.050,
              0.100, 0.170, 0.300, 0.400, 0.500, 0.650, 0.800, 0.900)
df <- data.frame(
  patient=1:18,
  dose = rep(c(1:4, 7), c(3, 4, 5, 4, 2)),
  tox = rep(0:1, c(16, 2)))
target <- 0.30
outcomes <- '1NNN 2NNNN 3NNNN 4NNNN 7TT'


# bcrm version
fit1 <- bcrm(stop = list(nmax=18),
             data = df,
             p.tox0 = skeleton,
             dose = dose,
             ff = "power",
             prior.alpha = list(3, 0, 1.34^2),
             target.tox = target,
             constrain = FALSE,
             sdose.calculate = "median",
             pointest = "mean")
print(fit1)
fit1$ndose[[1]]$mean

# trialr version
fit2 <- get_trialr_crm(skeleton = skeleton, target = target, model = 'empiric',
                       beta_sd = 1.34, seed = 2020) %>%
  fit(outcomes = outcomes)

# MTD matches?
expect_equal(fit1$ndose[[1]]$ndose, recommended_dose(fit2))

# mean_prob_tox matches?
epsilon <- 0.02
expect_true(all(abs(fit1$ndose[[1]]$mean - mean_prob_tox(fit2)) < epsilon))



# To check logistic2 stan_crm fit ----
# This example is somewhat contrived, using Neuenschwander et al.'s bivariate
# normal prior on the doses backwards imputed from the toxicity skeleton, not
# the log-scaled dose that Neuenschwander et al. intended.

# bcrm version
mu <- c(2.15, 0.52)
Sigma <- rbind(c(0.84^2, 0.134), c(0.134, 0.80^2))
# 0.84 * 0.8 * 0.2

mu <- c(-2, 0)
Sigma <- rbind(c(1, 0), c(0, 1))

fit1 <- bcrm(stop = list(nmax=18),
             data = df,
             p.tox0 = skeleton,
             dose = dose,
             ff = "logit2",
             prior.alpha = list(4, mu, Sigma),
             target.tox = target,
             constrain = FALSE,
             pointest = "mean",
             method = "rjags"
             # sdose.calculate = "median"
             )

fit1


# trialr version
# set.seed(2020)
# x <- exp(rnorm(10000, mean = 0.52, sd = 0.8))
# mean(x); sd(x)
# log(2.3)
fit2 <- get_trialr_crm(skeleton = skeleton, target = target,
                       model = 'logistic2',
                       alpha_mean = -2, alpha_sd = 1,
                       beta_mean = 0, beta_sd = 1,
                       seed = 2020) %>%
  fit(outcomes = outcomes)
mean_prob_tox(fit2)

# as.data.frame(fit2)

# MTD matches?
expect_equal(fit1$ndose[[1]]$ndose, recommended_dose(fit2))

# mean_prob_tox matches?
fit1$ndose[[1]]$mean
mean_prob_tox(fit2)

# median_prob_tox matches?
fit1$ndose[[1]]$quantiles['50%', ]
median_prob_tox(fit2)

plot(fit1$ndose[[1]]$mean - mean_prob_tox(fit2))

epsilon <- 0.1
expect_true(all(
  abs(fit1$ndose[[1]]$mean - mean_prob_tox(fit2)) %>% max
  < epsilon))

# WTF ????

# To check stan_nbg fit ----
dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
## Data from the first 5 cohorts of 18 patients
df <- data.frame(
  patient=1:18,
  dose=rep(c(1:4, 7), c(3, 4, 5, 4, 2)),
  tox=rep(0:1, c(16, 2)))
## Target toxicity level
target <- 0.30

## A 2-parameter model, using priors as specified in Neuenschwander et al 2008.
## Posterior mean used to choose the next dose
## Standardised doses using reference dose, 250mg
sdose <- log(dose / 250)
## Bivariate lognormal prior for two parameters
mu <- c(2.15, 0.52)
Sigma <- rbind(c(0.84^2, 0.134), c(0.134, 0.80^2))
## Using rjags (requires JAGS to be installed)
## Not run:
fit1 <- bcrm(stop = list(nmax=18),
                               data = df,
                               sdose = sdose,
                               dose = dose,
                               ff = "logit2",
                               prior.alpha = list(4, mu, Sigma),
                               target.tox = target,
                               constrain = FALSE,
                               pointest = "mean",
                               method = "rjags")
print(fit1)

# Reading values off the plot in lower right panel of Figure 1
nbg_post_mean = c(0.01, 0.02, 0.05, 0.13, 0.19, 0.25, 0.30, 0.35, 0.47, 0.53,
                  0.68, 0.74, 0.85, 0.89, 0.92)


library(trialr)
fit2 <- stan_nbg(outcome_str = outcomes,
                 real_doses = dose, d_star = 250, target = 0.3,
                 alpha_mean = 2.15, alpha_sd = 0.84,
                 beta_mean = 0.52, beta_sd = 0.8,
                 seed = 2020)
# trialr's mean and median disagree with Figure 1 in NBG too.

fit1$ndose[[1]]$mean
fit2$prob_tox

epsilon <- 0.02
expect_true(all(abs(fit1$ndose[[1]]$mean - fit2$prob_tox) < epsilon))
# But trialr agrees with bcrm

epsilon <- 0.04
expect_true(all(abs(nbg_post_mean - fit2$prob_tox) < epsilon))

summary(fit2, pars = c('alpha', 'beta'))$summary
