% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_paths.R
\name{graph_paths}
\alias{graph_paths}
\title{Visualise dose-paths as a graph}
\usage{
graph_paths(paths, viridis_palette = "viridis", RColorBrewer_palette = NULL)
}
\arguments{
\item{paths}{Object of type \code{\link{dose_paths}}}

\item{viridis_palette}{optional name of a colour palette in the
viridis package.}

\item{RColorBrewer_palette}{optional name of a colour palette in the
RColorBrewer package.}
}
\description{
Visualise dose-paths as a graph
}
\details{
The viridis package supports palettes: viridis, magma, plasma, inferno, and
cividis. The RColorBrewer package supports many palettes. Refer to those
packages on CRAN for more details.
}
\examples{
paths <- get_three_plus_three(num_doses = 5) \%>\%
  get_dose_paths(cohort_sizes = c(3, 3, 3))
\dontrun{
graph_paths(paths)
graph_paths(paths, viridis_palette = 'plasma')
graph_paths(paths, RColorBrewer_palette = 'YlOrRd')
}
}
