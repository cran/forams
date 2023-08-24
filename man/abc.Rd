\name{abc}
\alias{abc}

\title{
Abundance and Biomass Comparison Method
}
\description{
This function performs the ABC and W statistic calculation.
}
\usage{
  abc(df, Perm, confInt)
}

\arguments{
  \item{df}{
a numeric data frame containing the abundance and biomass as columns and \emph{taxa} as rows. NAs are not allowed.
}
  \item{Perm}{
the number of permutations to be realized for calculating the Confidence Interval.
}
  \item{confInt}{
the Confidence interval range (90\%, 95\% or 99\%, any other values will cause an error).
}
}
\details{
The function generates a list of cumulative percentage values of Abundance, Biomass and Biomass - Abundance for each \emph{taxon} (\cite{Warwick 1986}), which are used for W statistic (\cite{Warwick & Clarke 1994}) calculation and posterior k-dominance curve plotting.
}
\value{
An \code{abc} S4 object has the fallowing elements:

An abc slot with: 
  \item{Accum.Abun }{The cumulative percentage of abundance contribution.}
  \item{Accum.Biomass }{The cumulative percentage of biomass contribution.}
  \item{BiAi }{Biomass - Abundance calculation results.}
and a W.Stat slot with:
  \item{W.Stat }{The result of the W statistic calculation and its Confidence Interval.}
}
\references{
Warwick, R.M. (1986). A new method for detecting pollution effects on marine macrobenthic communities. \emph{Marine Biology} \bold{92} (4), 557-562.

Warwick, R.M., & Clarke, K.R. (1994). Relearning the ABC: taxonomic changes and abundance/biomass relationships in disturbed benthic communities. \emph{Marine Biology} \bold{118} (4), 739-744.
}
\author{
Rodrigo Aluizio
}
\note{
The list elementes are ordered according to \code{Accum.Abun}, but the calculation is based on the contribution decreasing order, independently of any of the variables or \emph{taxon}.
}

\examples{
data(NB)
MyABC <- abc(NB)
plot(MyABC)
}

\keyword{arith}
\keyword{math}