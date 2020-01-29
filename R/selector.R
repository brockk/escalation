
model_frame.selector <- function(selector, ...) {
  tibble::tibble(
    patient = seq(1, selector %>% num_patients()),
    cohort = selector %>% cohort(),
    dose = selector %>% doses_given(),
    tox = selector %>% tox()
  )
}

empiric_tox_rate.selector <- function(selector, ...) {
  return(selector %>% tox_at_dose() / selector %>% n_at_dose())
}

