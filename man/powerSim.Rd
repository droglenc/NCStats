\name{powerSim}
\alias{powerSim}
\title{Dynamic simulation of power calculation in 1-sample mean problems.}
\usage{
powerSim(mu0 = 100, s.mua = 95, s.sigma = 10, s.n = 30,
  s.alpha = 0.05, lower.tail = TRUE)
}
\arguments{
  \item{mu0}{A single numeric that is the null hypothesized
  mean.}

  \item{s.mua}{A single numeric that is the starting value
  for the actual population mean.}

  \item{s.sigma}{A single numeric that is the starting
  value for the actual population standard deviation.}

  \item{s.n}{A single numeric that is the starting value
  for the sample size.}

  \item{s.alpha}{A single numeric that is the starting
  value for alpha.}

  \item{lower.tail}{A single logical indicating if the
  rejection region is into the lower tail or not.}
}
\value{
None, but a dynamic graphic with slider bars is produced.
}
\description{
This function plots the null and actual distributions,
highlights the rejection region and the power region, and
allows the user to manipulate the effect size, standard
deviation, sample size, and alpha to determine the effect
on power.
}
\examples{
if (interactive()) {

powerSim()
powerSim(lower.tail=FALSE)

}
}
\keyword{dynamic}

