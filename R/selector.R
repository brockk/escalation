
model_frame.selector <- function(selector, ...) {
  tibble::tibble(
    patient = seq(1, selector %>% num_patients()),
    cohort = selector %>% cohort(),
    dose = selector %>% doses_given(),
    tox = selector %>% tox()
  )
}
