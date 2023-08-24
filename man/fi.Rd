\name{fi}
\alias{fi}

\title{
The FORAM Index
}
\description{
This function implements the FORAM Index (FI) in community abundance datasets.
}
\usage{
  fi(df, groups)
}

\arguments{
  \item{df}{
a numerical data frame with samples as columns and \emph{taxa} as rows.
}
  \item{groups}{
a three level grouping factor.
}
}
\details{
his analysis is directed for health evaluation and monitoring of reef environments (\cite{Hallock et al. 2003}) and it is based in foraminiferal total fauna methodology. 

The \emph{taxa} classification that determines the groups are originally based on genera, but species data from literature or experiments will be accepted as well. The grouping factor must be composed solely by \code{Ps} (simbiont-bearing), \code{Po} (opportunistic), or \code{Ph} (other small heterotrophic) levels. NAs are not allowed.

The plot uses the \code{axis} function, so a complete customization (i.e. \code{side}) of the axes is not possible at this moment, and some other parameters may show improperly if changed.
}
\value{
A \code{fi} object has the fallowing elements:

  \item{PlotOrder }{a numerical vector defining the sites plot order, only used for plotting.}
  \item{FI }{a numerical vector with the sites FORAM Index values.}
}
\references{
Hallock, P., Lidz, B.H., Cockey-Burkhard, E.M. & Donnelly, K.B. (2003). Foraminifera as bioindicators in coral reef assessment and monitoring: The foram index. \emph{Environmental monitoring and assessment} \bold{81}, 221-238.
}
\author{
Rodrigo Aluizio
}

\note{
FI > 4 indicates environment conducive to reef growth (CRG), FI varying between 3 and 5 indicates environmental change (Coefficient of Variation > 0.1), 2 < FI < 4 indicates environment marginal for reef growth (MRG) and unsuitable for recovery and FI < 2 indicates stressed conditions unsuitable for reef growth (UGR).

For more details on other graphic prameters see \code{\link[graphics]{plot.default}} and \code{\link[graphics]{par}}.
}

\examples{
data(LF)
data(Factors)
MyFI <- fi(LF, Factors)
plot(MyFI)
}

\keyword{arth}
\keyword{math}
