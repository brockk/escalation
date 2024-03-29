% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_empiric_crm_skeleton_weights.R
\name{get_empiric_crm_skeleton_weights}
\alias{get_empiric_crm_skeleton_weights}
\title{Get posterior model weights for several empiric CRM skeletons.}
\usage{
get_empiric_crm_skeleton_weights(
  skeletons,
  events_at_dose,
  n_at_dose,
  prior = rep(1, nrow(skeletons))
)
}
\arguments{
\item{skeletons}{matrix with one skeleton per row, so that the number of
columns is the number of doses under investigation.}

\item{events_at_dose}{integer vector of number of events at doses}

\item{n_at_dose}{integer vector of number of patients at doses}

\item{prior}{vector of prior model weights. Length should be same as number
of rows in \code{skeletons}. Default is equal weighting.}
}
\value{
numerical vector, posterior weights of the skeletons.
}
\description{
Get posterior model weights for several empiric CRM skeletons, assuming a
normal prior on the beta model parameter
}
