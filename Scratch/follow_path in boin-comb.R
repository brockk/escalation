
num_doses <- c(3, 4)
target <- 0.25
model <-
  follow_path("1.1N 1.2N 1.3N 2.1N 2.2N 2.3N") %>%
  get_boin_comb(num_doses = num_doses, target = target) %>%
  stop_at_n(n = 9)

x <- model %>% fit("1.1N")
recommended_dose(x)
# Following path, always c(1, 2)

x <- model %>% fit("1.1N 2.1N")
recommended_dose(x)
# Diverged from path by dose-choice, BOIN-COMB model takes over

x <- model %>% fit("1.1N 1.2N 2.1T")
recommended_dose(x)
# Diverged from path by outcome, BOIN-COMB model takes over
