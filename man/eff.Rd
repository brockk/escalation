% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{eff}
\alias{eff}
\title{Binary efficacy outcomes.}
\usage{
eff(x, ...)
}
\arguments{
\item{x}{Object of type \code{\link{selector}}.}

\item{...}{Extra args are passed onwards.}
}
\value{
an integer vector
}
\description{
Get a vector of the binary efficacy outcomes for evaluated patients.
}
\examples{
prob_select = c(0.1, 0.3, 0.5, 0.07, 0.03)
model <- get_random_selector(prob_select = prob_select,
                             supports_efficacy = TRUE)
x <- model \%>\% fit('1NTN 2EN 5BB')
eff(x)
}
