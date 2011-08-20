\name{abc}
\alias{abc}
\alias{abc.plot}
\title{
Abundance and Biomass Comparison Method
}
\description{
This function performs the ABC and W statistic calculation.
}
\usage{
abc(df)
abc.plot(X, xlim = c(0, ceiling(log(length(X$Accum.Abund)))),
  ylim = c(0, 100), yaxp = c(0, 100, 10), lty.bio = 'dotted',
  lty.abu = 'solid', lwd = 2, col.bio = 'black', col.abu = 'black',
  xlab = expression('Species Rank'~(Log[e]~Scale)),
  ylab = 'Cumulative Dominance \%', leg = TRUE, W = TRUE,
  col.pol = "#f5f5f5", \dots)
}
\arguments{
  \item{df}{
a numeric data frame containing the abundance and biomass as columns and \emph{taxa} as rows. NAs are not allowed.
}
  \item{X}{
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
The function generates a list of cumulative percentage values of Abundance, Biomass and Biomass - Abundance for each \emph{taxon} (\cite{Warwick 1986}), which are used for W statistic (\cite{Warwick & Clarke 1994}) calculation and posterior k-dominance curve plotting.

The plot produces a two lines plot with a ln abscissa and uses the \code{axis} function, so a complete customization (i.e. \code{side}) of the axes is not possible at this moment.
}
\value{
An \code{abc} object has the fallowing elements:

  \item{Accum.Abun }{The cumulative percentage of abundance contribution.}
  \item{Accum.Biomass }{The cumulative percentage of biomass contribution.}
  \item{BiAi }{Biomass - Abundance calculation results.}
  \item{W.Stat }{The result of the W statistic calculation.}
  \item{attr(, "row.names") }{\emph{Taxa} labels.}
}
\references{
Warwick, R.M. (1986). A new method for detecting pollution effects on marine macrobenthic communities. \emph{Marine Biology} \bold{92} (4), 557-562.

Warwick, R.M., & Clarke, K.R. (1994). Relearning the ABC: taxonomic changes and abundance/biomass relationships in disturbed benthic communities. \emph{Marine Biology} \bold{118} (4), 739-744.
}
\author{
Rodrigo Aluizio
}
\note{
The list elementes are ordered according to \code{Accum.Abun}, but the calculation is based on the contribution decreasing order, independently any of the variables or \emph{taxon}.
For more details on other graphic prameters such as line and box types see \code{\link{par}} and \code{\link{plot.default}}.
}
\examples{
data(NB)
MyABC <- abc(NB)
abc.plot(MyABC)
## To generate an taxa based data frame ordered alphabeticaly, use:
\dontrun{
Table <- as.data.frame(MyABC[1:3])
rownames(Table) <- attr(MyABC, 'row.names')
Table <- Table[sort(rownames(Table)),] 
## This table doesn't contain the result of the W statistic
}
}
\keyword{arith}
\keyword{math}
\keyword{dplot}
\keyword{hplot}
