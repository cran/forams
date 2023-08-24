\name{plot.abc}
\alias{plot.abc}
\alias{plot,abc-method}

\title{
Plot function for Abundance and Biomass Comparison Method objects.
}
\description{
This function is a customizable plot for the abundance and biomass lines from the ABC object.
}
\usage{
\S4method{plot}{abc}(x, xlim = c(0, ceiling(log(length(x@abc$Accum.Abund)))),
     ylim = c(0, 100), yaxp = c(0, 100, 10), lty.bio = 'dotted',
     lty.abu = 'solid', lwd = 2, col.bio = 'black', col.abu = 'black',
     xlab = expression('Species Rank'~(Log[e]~Scale)),
     ylab = 'Cumulative Dominance \%', leg = TRUE, W = TRUE, col.pol = '#f5f5f5',
     \dots)
}
\arguments{
  \item{x}{
an object of class \code{abc}.
}
  \item{xlim}{
the x limits (x1, x2) of the plot, defaults from 0 to the maximum value of x (in a log scale).
}
  \item{ylim}{
the y limits (x1, x2) of the plot, defaults to \code{c(0, 100)}
}
  \item{yaxp}{
a vector of the form c(y1, y2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks, defaults to \code{c(0, 100, 10)}.
}
  \item{lty.bio}{
line type of the biomass curve, defaults to \code{"dotted"}.
}
  \item{lty.abu}{
line type of the abundance curve, defaults to \code{"solid"}.
}
  \item{lwd}{
the line width, a positive number, defaulting to 2.
}
  \item{col.bio}{
the line color of the biomass curve, defaulting to \code{"black"}.
}
  \item{col.abu}{
the line color of the abundance curve, defaulting to \code{"black"}.
}
  \item{xlab}{
a label for the x axis, defaults to \code{expression('Species Rank'~(Log[e]~Scale))}.
}
  \item{ylab}{
a label for the y axis, defaults to \code{"Cumulative Dominance \%"}.
}
  \item{leg}{
logical; if TRUE draws a legend at the \code{bottomright} part of the plotting area, defaults to \code{TRUE}.
}
  \item{W}{
logical; if TRUE draws the W statistics value at the \code{topleft} part of the plotting area, defaults to \code{TRUE}.
}
  \item{col.pol}{
the polygon filling color, defaults to \code{"#f5f5f5"}.
}
  \item{\dots}{
other graphical parameters, see \code{\link{par}} for details.
}
}
\details{
The plot produces a two lines plot with a ln abscissa and uses the \code{axis} function, so a complete customization (i.e. \code{side}) of the axes is not possible at this moment.
}

\references{
Warwick, R.M. (1986). A new method for detecting pollution effects on marine macrobenthic communities. \emph{Marine Biology} \bold{92} (4), 557-562.

Warwick, R.M., & Clarke, K.R. (1994). Relearning the ABC: taxonomic changes and abundance/biomass relationships in disturbed benthic communities. \emph{Marine Biology} \bold{118} (4), 739-744.
}

\author{Rodrigo Aluizio}

\note{For more details on other graphic prameters such as line and box types see \code{\link{par}} and \code{\link{plot.default}}.}

\examples{
data(NB)
MyABC <- abc(NB)
plot(MyABC)
}

\keyword{dplot}
\keyword{hplot}
