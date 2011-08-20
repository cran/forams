\name{forams-package}
\alias{forams-package}
\alias{forams}
\docType{package}
\title{
Foraminifera and Community Ecology Analyses
}
\description{
SHE, FORAM Index and ABC Method analyses and custom plot functions for community data.
}
\details{

This package always uses data frames with \emph{taxa} as rows and sites or variables as columns. It also provides customizable plot functions to use with the objects resulting from the analyses, and randomly generated example datasets.

The analyses performed by this package are: SHE, FORAM Index and ABC Method.

\tabular{ll}{
Package: \tab forams\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-08-20\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Rodrigo Aluizio

Maintainer: Rodrigo Aluizio <r.aluizio@gmail.com>
}
\references{
Buzas, M.A. & Hayek, L.A.C. (1998). SHE analysis for biofacies identification. \emph{Journal of Foraminiferal Research} \bold{28} (3), 233-239.

Hallock, P., Lidz, B.H., Cockey-Burkhard, E.M. & Donnelly, K.B. (2003). Foraminifera as bioindicators in coral reef assessment and monitoring: The foram index. \emph{Environmental monitoring and assessment} \bold{81}, 221-238.

Warwick, R.M. (1986). A new method for detecting pollution effects on marine macrobenthic communities. \emph{Marine Biology} \bold{92} (4), 557-562.

Warwick, R.M., & Clarke, K.R. (1994). Relearning the ABC: taxonomic changes and abundance/biomass relationships in disturbed benthic communities. \emph{Marine Biology} \bold{118} (4), 739-744.

Wilson, B., Dawe, R., Gopee, A., Grant, S., Kissoon, A., Young, T., Noon, C., McLean, A. & Singh, K. (2010). Determining Boundaries between Abundance Biozones Using Minimal Equipment. \emph{International Journal of Ecology} \bold{2010}, 1-14.
}
\keyword{ package }
\seealso{
 \code{\link{abc}}, \code{\link{fi}}, \code{\link{she}}
}
\examples{
data(NB)
data(Factors)
data(LF)
# ABC
abc.plot(abc(NB))
# FORAM Index
fi.plot(fi(LF, Factors))
# SHE
she.plot(she(LF))
}
