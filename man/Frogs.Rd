% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Frogs.R
\docType{data}
\name{Frogs}
\alias{Frogs}
\title{Numbers of frogs on lakes as determined by call and visual methods.}
\format{
A data frame with 1000 observations on the following 2 variables:
  \describe{
    \item{call}{Abundance determined from the \sQuote{call} method.}
    \item{visual}{Abundance determined from the \sQuote{visual} method.}
  }
}
\source{
Randomly determined from realistic values provided by the Sigurd Olson Environmental Institute, Northland College, Ashland, WI.
}
\description{
Hypothetical population of numbers of frogs on lakes as determined by call and visual methods.
}
\note{
Data were randomly determined using the following code:

require(MASS)
round(mvrnorm(n=1000,c(120,225),matrix(c(25,18,18,35),2,2),empirical=TRUE),0)
}
\keyword{datasets}
