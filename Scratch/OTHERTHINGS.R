get_mtpi <- function(num_doses, target, ...) {

  x <- list(
    num_doses = num_doses,
    target = target,
    # model = model,

    # k1 = k1,
    # k2 = k2,

    extra_args = list(...)
  )

  class(x) <- c('mtpi_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}

get_mtpi2 <- function(num_doses, target, ...) {

  x <- list(
    num_doses = num_doses,
    target = target,
    # model = model,

    # k1 = k1,
    # k2 = k2,

    extra_args = list(...)
  )

  class(x) <- c('mtpi2_selector_factory',
                'tox_selector_factory',
                'selector_factory')
  return(x)
}


# # Pertinent to mTPI:
# ei_lower <- max(target - k1 * post_sd, 0)
# ei_upper <- min(target + k2 * post_sd, 1)
# prob_ui <- pbeta(ei_lower, 1, 1000)
# if(obs_tox_rate >= ei_lower & obs_tox_rate <= ei_upper) {
#   # Stick at last dose
#   recommended_dose <- last_dose
#   continue <- TRUE
# } else if(obs_tox_rate < ei_lower) {
#   # De-escalate if possible
#   recommended_dose <- max(1, last_dose - 1)
#   continue <- TRUE
# } else if(obs_tox_rate > ei_upper) {
#   # Escalate if possible
#   recommended_dose <- min(num_doses, last_dose + 1)
# } else {
#   # TODO
# }
