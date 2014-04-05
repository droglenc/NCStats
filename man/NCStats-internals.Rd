\name{NCStats-internals}
\alias{.onAttach}
\alias{NCStats-internals}
\alias{assumPlot_ANOVA}
\alias{assumPlot_REGRESS}
\alias{c.region}
\alias{cCDF.plot}
\alias{cPDF.plot}
\alias{d.region}
\alias{dCDF.plot}
\alias{dPDF.plot}
\alias{lblADTest}
\alias{lblLevTest}
\alias{lblNCVTest}
\alias{lblOutTest}
\title{Internal functions used in NCStats.}
\usage{
.onAttach(lib, pkg, ...)
}
\description{
Internal functions used in NCStats
}
\details{
These functions perform code required by other functions in
the NCStats package.  These functions are not to be called
by the user.
}
\note{
Take note of the following uses: \itemize{ \item
\code{c.region} and \code{d.region} are used in
\code{\link{distrib}}, \code{\link{plot.htest}} and
\code{\link{powerSim}}. \item \code{cCDF.plot},
\code{cPDF.plot}, \code{dCDF.plot}, and \code{dPDF.plot}
are all used in the distribution simulators -- e.g.,
\code{\link{sbeta}}, \code{\link{snorm}}, etc.. \item
\code{assumPlot_ANOVA}, \code{assumPlot_REGRESS},
\code{lblADTest}, \code{lblNCVTest}, \code{lblOutTest}, and
\code{lblLevTest} functions are all used in
\code{\link{transChooser}}. }
}
\keyword{internal}

