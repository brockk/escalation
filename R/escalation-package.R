#' The 'escalation' package.
#'
#' @description
#' escalation provides methods for working with dose-finding clinical trials.
#' We provide implementations of many dose-finging clinical trial designs,
#' including the continual reassessment method (CRM) by O'Quigley et al. (1990)
#' <doi:10.2307/2531628>, the toxicity probability interval (TPI) by Ji et al.
#' (2007) <doi:10.1177/1740774507079442>, the modified TPI (mTPI) design by Ji
#' et al. (2010) <doi:10.1177/1740774510382799>, the Bayesian optimal interval
#' design (BOIN) by Liu & Yuan (2015) <doi:10.1111/rssc.12089>, and the 3+3
#' described by Korn et al. (1994) <doi:10.1002/sim.4780131802>. All designs
#' are implemented with a common interface. We also offer optional additional
#' classes to tailor the behaviour of all designs, including avoiding skipping
#' doses, stopping after n patients have been treated at the recommended dose,
#' stopping when a toxicity condition is met, or demanding that n patients are
#' treated before stopping is allowed. By daisy-chaining together these classes
#' using the pipe operator from 'magrittr', it is simple to tailor the
#' behaviour of a dose-finding design so it behaves how the trialist wants.
#' Having provided a flexible interface for specifying designs, we then provide
#' functions to run simulations and calculate dose-pathways for future cohorts
#' of patients.
#'
#' @docType package
#' @name escalation-package
#' @aliases escalation
NULL
