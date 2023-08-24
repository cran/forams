\name{plot.she}
\alias{plot.she}
\alias{plot,she-method}

\title{
Custom plot function for SHE Analysis objects
}

\description{
This function generates customizable graphics for objects of the class \code{she}.
}

\usage{
\S4method{plot}{she}(x, pch = 20, pcol = 'black', pcex = 1, pbg = 'black', lcol = 'black',
  lwd = 1, lty = 'dotted', ylab = expression('ln'~italic(E)), bty = 'l', \dots)
}

\arguments{
  \item{x}{
an object of class \code{she}.
}
  \item{pch}{
plotting 'character', i.e., symbol to use with the points, defaults to \code{20}.
}
  \item{pcol}{
color code or name for plot symbols, defaults to \code{"black"}.
}
  \item{pcex}{
character (or symbol) expansion: a numerical vector, defaults to \code{1}.
}
  \item{pbg}{
background (fill) color for the open plot symbols given by pch=21:25, defaults to \code{"black"}
}
  \item{lcol}{
a specification for the default plotting color of the line, defaults to \code{"black"}.
}
  \item{lwd}{
the line width, a positive number, defaulting to \code{1}.
}
  \item{lty}{
the line type. Line types can either be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses 'invisible lines' (i.e., does not draw them). 
}
  \item{ylab}{
a label for the y axis, defaults to \code{expression("ln" ~ italic(E))}.
}
  \item{bty}{
box type drawn about the plot, defaults to \code{"l"}. Other types will not reproduce ideally.
}
  \item{\dots}{
other parameters to be passed to \code{\link{plot.default}}.
}
}
\details{
The plot produces a line plot with points on a ln abscissa and uses the \code{axis} function, so a complete customization (i.e. \code{side}) of the axes is not possible at this moment.
}

\references{
Buzas, M.A. & Hayek, L.A.C. (1998). SHE analysis for biofacies identification. \emph{Journal of Foraminiferal Research} \bold{28} (3), 233-239.

Wilson, B., Dawe, R., Gopee, A., Grant, S., Kissoon, A., Young, T., Noon, C., McLean, A. & Singh, K. (2010). Determining Boundaries between Abundance Biozones Using Minimal Equipment. \emph{International Journal of Ecology} \bold{2010}, 1-14.
}

\author{
Rodrigo Aluizio
}

\note{
For more details on other graphic prameters see \code{\link{par}}.
}

\examples{
data(LF)
MySHE <- she(LF, "abun")
plot(MySHE)
}


\keyword{dplot}
\keyword{hplot}