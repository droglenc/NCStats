\docType{data}
\name{SquareLakePopn}
\alias{SquareLakePopn}
\title{Biological characteristics of the "Square Lake" population.}
\format{A data frame of 1014 observations on the following 6 variables:
\describe{ \item{tl}{Total length (mm) of an individual fish.} \item{fl}{Fork
length (mm) of an individual fish.} \item{sl}{Standard length (mm) of an
individual fish.} \item{w}{Weight (g) of an individual fish.} \item{age}{Age
of an individual fish.} \item{sex}{Sex of an individual fish (\code{F}=Female
and \code{M}=Male).} }}
\description{
Simulated data for the population of 1014 fish in
\dQuote{Square Lake}.  Set random seed to 1 to extract
random sample used in the Introductory Statistics chapter
of FSA.
}
\section{Topic(s)}{
  \itemize{ \item Length frequency \item Length-weight
  \item Length conversion \item Size-at-age growth model }
}
\examples{
data(SquareLakePopn)
str(SquareLakePopn)
head(SquareLakePopn)
}
\keyword{datasets}

