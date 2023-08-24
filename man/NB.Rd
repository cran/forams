\name{NB}
\alias{NB}
\docType{data}
\title{
ABC Method Dataset
}
\description{
An example dataset containing some \emph{taxa} abundances and Biomasses to use in exemples of the \code{\link[forams]{abc}} function.
}
\usage{data(NB)}
\format{
  A data frame with 316 \emph{taxa} on the following 2 variables.
  \describe{
    \item{N}{a numeric vector with abundance data.}
    \item{Biomass}{a numeric vector with biomass data.}
  }
}
\details{
This dataset is an artificial random generated example. Unfortunately at the moment, due to authorship issues I can not present any real one. This may change in future versions.
}
\examples{
data(NB)
str(NB)
}
\keyword{datasets}
