\name{she}
\alias{she}

\title{
SHE Analysis
}
\description{
This function implements the SHE method in community abundance datasets.
}
\usage{
  she(df, method)
}

\arguments{
  \item{df}{
a numerical data frame with samples as columns and \emph{taxa} as rows.
}
  \item{method}{
the method to be used, (\code{"abundance"} or \code{"frequency"}), defaults to \code{"abundance"}.
}
}
\details{
This method is intended to determine boundaries between abundance biozones, based in raw abundance (SHEbi) or in frequency (SHEbip) (\cite{Buzas et al. 1998, Wilson et al. 2010}).
The custom plot produces a line plot with points on a ln abscissa and uses the \code{axis} function, so a complete customization (i.e. \code{side}) of the axes is not possible at this moment.
}
\value{
 \item{S }{richness values.}
 \item{H }{shannon diversity values.}
 \item{E }{equitability values.}
 \item{N or L }{number of specimens (N) or sites rank based on specimens frequency (L).}
}
\references{
Buzas, M.A. & Hayek, L.A.C. (1998). SHE analysis for biofacies identification. \emph{Journal of Foraminiferal Research} \bold{28} (3), 233-239.

Wilson, B., Dawe, R., Gopee, A., Grant, S., Kissoon, A., Young, T., Noon, C., McLean, A. & Singh, K. (2010). Determining Boundaries between Abundance Biozones Using Minimal Equipment. \emph{International Journal of Ecology} \bold{2010}, 1-14.
}
\author{
Rodrigo Aluizio
}
\note{
This function implements great part of the process, but a small part must be caried out for the researcher when defining where to cut biozones before reruning the test.
For more details on other graphic prameters see \code{\link[graphics]{par}}.
}
\examples{
data(LF)
MySHE <- she(LF, "abun")
plot(MySHE)
}

\keyword{math}
\keyword{arth}
