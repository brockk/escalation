
# Are sims the same? Test default Shiny sc. ----
design <- get_mtpi2(num_doses = 6, target = 0.3,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95,
                    alpha = 1, beta = 1) %>%
  stop_at_n(n = 30) %>%
  select_mtpi2_mtd(when = "finally", exclusion_certainty = 0.95)
sc1 <- simulate_trials(
  design,
  num_sims = 1000,
  true_prob_tox = c(0.05, 0.1, 0.2, 0.25, 0.3, 0.35)
)
sc1 # Differences at d6, for e.g.


# Check iso regression ----
path = "1NNN 2TNNNNN 3NNN 4NNNNNN 5NNN 6TTNNNN"
model_fitter <-
  get_mtpi2(num_doses = 6, target = 0.3,
            epsilon1 = 0.05, epsilon2 = 0.05,
            exclusion_certainty = 0.95,
            alpha = 1, beta = 1) %>%
  # follow_path(path = path) %>%
  stop_at_n(n = 27) %>%
  select_mtpi2_mtd(
    when = 'finally', target = 0.3,
    exclusion_certainty = 0.95,
    # alpha = 0.001, beta = 0.001
    alpha = 1, beta = 1
  )
x
# Fix when alpha and (or?) beta = 0
x <- model_fitter %>% fit(path)
continue(x)
recommended_dose(x) # TODO fix me
mean_prob_tox(x)
x %>% mean_prob_tox() # Breaks
x$parent %>% mean_prob_tox() # OK
x$parent$parent %>% mean_prob_tox() # OK

pava <- function(x, wt = rep(1, length(x))) {
  n <- length(x)
  if (n <= 1)
    return(x)
  if (any(is.na(x)) || any(is.na(wt))) {
    stop("Missing values in 'x' or 'wt' is not allowed")
  }
  lvlsets <- 1:n
  repeat {
    viol <- as.vector(diff(x)) < 0
    if(!(any(viol)))
      return(x)

    i <- min((1:(n - 1))[viol])
    lvl1 <- lvlsets[i]
    lvl2 <- lvlsets[i + 1]
    ilvl <- (lvlsets == lvl1 | lvlsets == lvl2)
    x[ilvl] <- sum(x[ilvl] * wt[ilvl])/sum(wt[ilvl])
    lvlsets[ilvl] <- lvl1
  }
  x
}
# mean_prob_tox.mtpi2_mtd_dose_selector
post_mean = (x$alpha + tox_at_dose(x)) / (x$alpha + x$beta + n_at_dose(x))
post_alpha = x$alpha + tox_at_dose(x)
post_beta = x$beta + n_at_dose(x) - tox_at_dose(x)
post_var = ((post_alpha) * (post_beta)) /
  ( (post_alpha + post_beta)^2 * (post_alpha + post_beta + 1) )
round(post_var, 3)
post_mean = pava(post_mean, wt = 1 / post_var)
x = post_mean
wt = 1 / post_var

# Reproduce Shiny iso.est
post_alpha = 0 + tox_at_dose(x)
post_beta = 0 + n_at_dose(x) - tox_at_dose(x)
post_mean = (post_alpha) / (post_alpha + post_beta)
post_var = ((post_alpha) * (post_beta)) /
  ( (post_alpha + post_beta)^2 * (post_alpha + post_beta + 1) )
post_var[post_var == 0] <- 0.03#max(post_var)
round(post_var, 3)
xyz = pava(post_mean, wt = 1 / post_var)
xyz
# The AZ app:
# 1) does not use the prior alpha & beta when calculating prob_tox for pava
# 2) implements a manual fix to avoid zero variance (suspect using max(var)),
#    a prob that exists because alpha and beta not used.

pava(c(0, 1/6, 0, 0, 0, 1/3))
pava(c(0, 1/6, 0, 0, 0, 1/3), wt = c())

# Check sims in mTPI2 paper ----

# Sc1 of Guo et al
design <- get_mtpi2(num_doses = 6, target = 0.3,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95,
                    alpha = 1, beta = 1) %>%
  stop_at_n(n = 30) %>%
  select_mtpi2_mtd(when = "finally", exclusion_certainty = 0.95)
sc1 <- simulate_trials(
  design,
  num_sims = 5000,
  true_prob_tox = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.25)
)
sc1 # 5% off
design_ <- get_mtpi2(num_doses = 6, target = 0.3,
                    epsilon1 = 0.05, epsilon2 = 0.05,
                    exclusion_certainty = 0.95,
                    alpha = 1, beta = 1) %>%
  stop_at_n(n = 30)
sc1_ <- simulate_trials(
  design_,
  num_sims = 5000,
  true_prob_tox = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.25)
)
sc1_

# Sc2 of Guo et al
sc2 <- simulate_trials(
  design,
  num_sims = 5000,
  true_prob_tox = c(0.04, 0.06, 0.08, 0.10, 0.3, 0.6)
)
sc2 # Matches

# Sc3 of Guo et al
sc3 <- simulate_trials(
  design,
  num_sims = 5000,
  true_prob_tox = c(0.01, 0.05, 0.10, 0.60, 0.70, 0.90)
)
sc3 # Similar

# Sc4 of Guo et al
sc4 <- simulate_trials(
  design,
  num_sims = 5000,
  true_prob_tox = c(0.25, 0.27, 0.29, 0.31, 0.33, 0.35)
)
sc4 # Similar

# Sc5 of Guo et al
sc5 <- simulate_trials(
  design,
  num_sims = 5000,
  true_prob_tox = c(0.35, 0.45, 0.50, 0.60, 0.70, 0.80)
)
sc5 # Different

# Sc6 of Guo et al
sc6 <- simulate_trials(
  design,
  num_sims = 5000,
  true_prob_tox = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
)
sc6 # Different



# test_that('select_mtpi2_mtd works like it should.', {

  # Reproduce decisions conveyed in Figure 3 in Guo et al. (2017)
  # at https://doi.org/10.1016/j.cct.2017.04.006

  num_doses <- 5
  target <- 0.3

  model <- get_mtpi2(num_doses = num_doses, target = target,
                     epsilon1 = 0.05, epsilon2 = 0.05,
                     exclusion_certainty = 0.95)



  # Tests at dose 1

  # Three patients treated
  fit <- model %>% fit('1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  # Six patients treated
  fit <- model %>% fit('1NNN 1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  # Nine patients treated
  fit <- model %>% fit('1NNN 1NNN 1NNN')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1NTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNT 1TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NNT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1TTT 1TTT 1TTT')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  # Ten patients treated
  fit <- model %>% fit('1NNN 1NNN 1NNN 1N')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1NNN 1T')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1NNT 1T')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1NTT 1T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNN 1TTT 1T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NNT 1TTT 1T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1NTT 1TTT 1T')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NNN 1TTT 1TTT 1T')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NNT 1TTT 1TTT 1T')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1NTT 1TTT 1TTT 1T')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))

  fit <- model %>% fit('1TTT 1TTT 1TTT 1T')
  expect_true(is.na(recommended_dose(fit)))
  expect_false(continue(fit))
  expect_equal(dose_admissible(fit), rep(FALSE, num_doses(fit)))


  # Tests at dose 2

  # Three patients treated
  fit <- model %>% fit('2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  # Six patients treated
  fit <- model %>% fit('2NNN 2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNT')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))
  # Note - this dose selection is an example of where mTPI and mTPI2 diverge

  fit <- model %>% fit('2NNT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2NTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  # Nine patients treated
  fit <- model %>% fit('2NNN 2NNN 2NNN')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2NNT')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2NTT')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2TTT')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2NNN 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2NNT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2NTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2TTT 2TTT 2TTT')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  # Ten patients treated
  fit <- model %>% fit('2NNN 2NNN 2NNN 2N')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2NNN 2T')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2NNT 2T')
  expect_equal(recommended_dose(fit), 3)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2NTT 2T')
  expect_equal(recommended_dose(fit), 2)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNN 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NNT 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('2NNN 2NTT 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2NNN 2TTT 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2NNT 2TTT 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('1NTT 2TTT 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  fit <- model %>% fit('2TTT 2TTT 2TTT 2T')
  expect_equal(recommended_dose(fit), 1)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, FALSE, FALSE, FALSE, FALSE))


  # Tests at top dose (i.e. escalation impossible)

  # Three patients treated
  fit <- model %>% fit('5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  # Six patients treated
  fit <- model %>% fit('5NNN 5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  # Nine patients treated
  fit <- model %>% fit('5NNN 5NNN 5NNN')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5NNT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5NTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5TTT')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NNN 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NNT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5TTT 5TTT 5TTT')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  # Ten patients treated
  fit <- model %>% fit('5NNN 5NNN 5NNN 5N')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5NNN 5T')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5NNT 5T')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5NTT 5T')
  expect_equal(recommended_dose(fit), 5)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNN 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NNT 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), rep(TRUE, num_doses(fit)))

  fit <- model %>% fit('5NNN 5NTT 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NNN 5TTT 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NNT 5TTT 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5NTT 5TTT 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

  fit <- model %>% fit('5TTT 5TTT 5TTT 5T')
  expect_equal(recommended_dose(fit), 4)
  expect_true(continue(fit))
  expect_equal(dose_admissible(fit), c(TRUE, TRUE, TRUE, TRUE, FALSE))

})
