% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{empiric_eff_rate}
\alias{empiric_eff_rate}
\title{Observed efficacy rate at each dose.}
\usage{
empiric_eff_rate(x, ...)
}
\arguments{
\item{x}{Object of class \code{\link{selector}}}

\item{...}{arguments passed to other methods}
}
\value{
a numerical vector
}
\description{
Get the empirical or observed efficacy rate seen at each dose under
investigation. This is simply the number of efficacies divded by the number
of patients evaluated.
}
\examples{
prob_select = c(0.1, 0.3, 0.5, 0.07, 0.03)
model <- get_random_selector(prob_select = prob_select,
                             supports_efficacy = TRUE)
x <- model \%>\% fit('1NTN 2EN 5BB')
empiric_tox_rate(x)
}
