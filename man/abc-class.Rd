\name{abc-class}
\Rdversion{1.1}
\docType{class}
\alias{abc-class}

\title{Class \code{"abc"}}
\description{
Class used to store \code{"abc"} analysis objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("abc", ...)}.
This class is composed by two slots, the first one stores a table with the abc analysis results and the second one the W Statistic result and its Confidence Interval.
}
\section{Slots}{
  \describe{
    \item{\code{abc}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{W.Stat}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "abc")}: ... }
	 }
}

\author{
Rodrigo Aluizio
}

\seealso{
See Also as \code{\link[forams]{abc}}}

\examples{
showClass("abc")
}

\keyword{classes}