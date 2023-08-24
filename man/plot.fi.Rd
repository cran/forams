\name{plot.fi}
\alias{plot.fi}
\alias{plot,fi-method}

\title{
Custom plot function for FORAM Index results
}

\description{
This function generates custom plots for FI objects.
}

\usage{
\S4method{plot}{fi}(x, ylim = c(1, 10), yaxp = c(1, 10, 9), xlab = 'Samples',
  ylab = 'FORAM Index', pch.urg = 25, pch.mrg = 21, pch.crg = 24,
  bg.urg = 'red', bg.mrg = 'yellow', bg.crg = 'green', pt.cex = 1,
  limits = TRUE, \dots)
}

\arguments{
  \item{x}{
an object of class \code{fi}.
}
  \item{ylim}{
the y limits (x1, x2) of the plot, defaults to \code{c(1, 10)}
}
  \item{yaxp}{
a vector of the form c(y1, y2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks, defaults to \code{c(1, 10, 9)}.
}
  \item{xlab}{
a label for the x axis, defaults to \code{"Samples"}.
}
  \item{ylab}{
a label for the y axis, defaults to \code{"FORAM Index"}.
}
  \item{pch.urg}{
plotting 'character', i.e., symbol to use, prefer one between 21:25, defaults to \code{25}
}
  \item{pch.mrg}{
plotting 'character', i.e., symbol to use, prefer one between 21:25, defaults to \code{21}
}
  \item{pch.crg}{
plotting 'character', i.e., symbol to use, prefer one between 21:25, defaults to \code{24}
}
  \item{bg.urg}{
background (fill) color for the open plot symbols given by \code{pch=21:25}, defaults to \code{"red"}.
}
  \item{bg.mrg}{
background (fill) color for the open plot symbols given by \code{pch=21:25}, defaults to \code{"yellow"}.
}
  \item{bg.crg}{
background (fill) color for the open plot symbols given by \code{pch=21:25}, defaults to \code{"green"}.
}
  \item{pt.cex}{
character (or symbol) expansion: a numerical vector, defaults to 1.
}
  \item{limits}{
logical; if TRUE, draws lines limiting the three FORAM Index classes and identifies them, defaults to \code{TRUE}.
}
  \item{\dots}{
other graphical parameters, see \code{\link{par}} for details.
}
}
\details{
The plot uses the \code{axis} function, so a complete customization (i.e. \code{side}) of the axes is not possible at this moment, and some other parameters may show improperly if changed.
}

\references{
Hallock, P., Lidz, B.H., Cockey-Burkhard, E.M. & Donnelly, K.B. (2003). Foraminifera as bioindicators in coral reef assessment and monitoring: The foram index. \emph{Environmental monitoring and assessment} \bold{81}, 221-238.
}

\author{
Rodrigo Aluizio
}

\note{
FI > 4 indicates environment conducive to reef growth (CRG), FI varying between 3 and 5 indicates environmental change (Coefficient of Variation > 0.1), 2 < FI < 4 indicates environment marginal for reef growth (MRG) and unsuitable for recovery and FI < 2 indicates stressed conditions unsuitable for reef growth (UGR).

For more details on other graphic prameters see \code{\link{plot.default}} and \code{\link{par}}.
}

\examples{
data(LF)
data(Factors)
MyFI <- fi(LF, Factors)
plot(MyFI)
}

\keyword{dplot}
\keyword{hplot}
