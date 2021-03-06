% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc-utils.R
\name{ci.t}
\alias{ci.t}
\title{Constructs confidence intervals assuming normal distribution.}
\usage{
ci.t(est, SE, obsdf, conf.level = 0.95)
}
\arguments{
\item{est}{A value that estimates a parameter (i.e., a statistic)}

\item{SE}{The standard error of the estimate}

\item{obsdf}{The degrees-of-freedom}

\item{conf.level}{The level of confidence as a decimal}
}
\value{
Returns a matrix containing the lower and upper values of the confidence interval.
}
\description{
Returns the confidence interval endpoints given an estimate, SE, and df assuming that the estimate follows a normal distribution so that the t-distribution can be used when constructing the CI.
}
\examples{

ci.t(2.96,0.32,14)

}
\seealso{
\code{\link[FSA:nlsBoot]{confint.nlsBoot}} in \pkg{FSA}.
}
\keyword{htest}
