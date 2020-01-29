
model_frame.selector <- function(selector, ...) {
  tibble::tibble(
    dose = selector %>% doses_given(),
    tox = selector %>% tox()
  )
}
