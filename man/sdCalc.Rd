% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sims-teaching.R
\name{sdCalc}
\alias{sdCalc}
\alias{print.sdCalc}
\title{Shows the steps in the manual calculation of the standard deviation.}
\usage{
sdCalc(x, digits = getOption("digits"))

\method{print}{sdCalc}(x, ...)
}
\arguments{
\item{x}{A numeric vector}

\item{digits}{A numeric indicating the number of decimals to round the numeric summaries to.  If left at \code{NULL} (default) then the number of digits will be obtained from \code{getOption('digits')}}

\item{\dots}{Other arguments to the generic \code{print} functions (not currently used)}
}
\value{
A list containing the sample size (\code{n}), sample mean (\code{mean}), data.frame of intermediate calculations (\code{tbl}), and number of digits to print (\code{digits))}.
}
\description{
Shows the steps in the manual calculation of the standard deviation.
}
\note{
This function shows a table of intermediate output in the calculation of the standard deviation.  Used purely to demonstrate the hand-calculation of the standard deviation.  Use \code{\link[stats]{sd}} to actually compute the standard deviation.
}
\examples{
## Numeric vector
y <- runif(8)
# typical computation
sd(y)   
# this function           
sdCalc(y)            
# this function, controlling the number of digits
sdCalc(y,digits=4)

}
\seealso{
\code{\link[stats]{sd}}
}
\keyword{misc}
