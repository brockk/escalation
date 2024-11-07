
library(escalation)
library(tidyverse)

dose_escalation_table<-function(
    mod,
    stop_at_n_in_dose_level,
    dose = 2,
    stop_label = "DU"
){

  all_perms <- data.frame(
    number_dlts = rep(
      0:stop_at_n_in_dose_level,
      each = stop_at_n_in_dose_level + 1
    ),
    total_n = rep(
      0:stop_at_n_in_dose_level,
      stop_at_n_in_dose_level + 1
    )
  ) %>%
    mutate(number_no_dlts = total_n - number_dlts)

  for (i in 1:(stop_at_n_in_dose_level + 1)^2) {
    if (all_perms$number_no_dlts[i] >= 0 &
        (all_perms$number_dlts[i] > 0 | all_perms$total_n[i] > 0)) {
      # Construct outcome string
      all_perms[i, 4] <- paste0(
        dose,
        paste0(rep("T", all_perms$number_dlts[i]), collapse = ""),
        paste0(rep("N", all_perms$number_no_dlts[i]), collapse = "")
      )
      x <- mod %>% fit(all_perms[i, 4])
      all_perms[i, 5] <- recommended_dose(x)
      if (!((dose_admissible(x))[dose])) {
        all_perms[i, 5] <- 0
      }
      if (all_perms[i, 5] > dose) {
        all_perms[i, 6] <- "E"
      } else if (all_perms[i, 5] == dose) {
        all_perms[i, 6] <- "S"
      } else if (all_perms[i, 5] == 0) {
        all_perms[i, 6] <- stop_label
      } else if (all_perms[i, 5] < dose) {
        all_perms[i, 6] <- "D"
      }
    } else {
      all_perms[i, 4] <- NA
      all_perms[i, 5] <- NA
      all_perms[i, 6] <- ""
    }
  }
  colnames(all_perms) <- c(
    "dlts", "cohort_n", "non_dlts", "string", "decision", "out"
  )
  sub_df <- all_perms %>%
    filter(cohort_n == stop_at_n_in_dose_level & out == stop_label)
  if(nrow(sub_df) > 0) {
    maxval <- min(sub_df$dlts)
    all_perms <- all_perms %>% filter(dlts <= maxval)
  } else {
    maxval <- max(all_perms$dlts, na.rm = TRUE)
  }
  plot(
    all_perms$dlts ~ all_perms$cohort_n,
    ylim = c(maxval, 0), cex = 0,
    axes = FALSE, ylab = "Number of DLTs", xlab = ""
  )
  axis(side = 3, at = c(1:stop_at_n_in_dose_level), line = 0)
  axis(side = 2, at = c(0:maxval), las = 2, line = 0)
  text(x = all_perms$cohort_n, y = all_perms$dlts, label = all_perms$out)
  box()
  mtext("Number of subjects at current dose", side = 3, line = 3)
}

mymod <- get_mtpi2(
  num_doses = 9,
  target = 0.4,
  alpha = 2,
  beta = 2,
  epsilon1 = 0.15,
  epsilon2 = 0.03,
  exclusion_certainty = 0.8
)
mymod <- get_dfcrm(skeleton = c(0.1, 0.2, 0.33, 0.7, 0.9), target = 0.33)
dose_escalation_table(
  mod = mymod,
  stop_at_n_in_dose_level = 10,
  dose = 3
)
