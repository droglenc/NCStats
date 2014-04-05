\name{predictPlot}
\alias{predictPlot}
\alias{predictionPlot}
\title{Shows the predicted value and interval on a fitted line plot.}
\usage{
predictPlot(...)

predictionPlot(mdl, newdata, interval = "prediction", conf.level = 0.95,
  lty = 1, lwd = 3, ...)
}
\arguments{
  \item{mdl}{an \code{lm} or \code{nls} object (i.e.,
  returned from fitting a model with either \code{lm} or
  \code{nls}).}

  \item{newdata}{A data frame in which to look for
  variables with which to predict.  This cannot be omitted
  as it is with \code{predict}.}

  \item{interval}{a string indicating whether to plot
  confidence (\code{="confidence"}) or prediction
  (\code{="prediction"}) intervals.}

  \item{conf.level}{a decimal numeric indicating the level
  of confidence to use for confidence and prediction
  intervals.  Default is \code{0.95}.}

  \item{lty}{a numeric indicating the type of line used for
  representing the intervals.  See \code{par}.}

  \item{lwd}{a numeric indicating the width of line used
  for representing the intervals.  See \code{par}.}

  \item{\dots}{Other arguments to the \code{fitPlot}
  function.  For example, include \code{legend=TRUE} to
  include a legend on the fitted line plot for an IVR.}
}
\value{
A data frame is returned that contains the number of the
new observation (for comparison to the graphic that is
produced), the values of the variables in \code{newdata},
and the predicted values at those observed values.
}
\description{
Shows the predicted value and interval on a fitted line
plot.  This function is used to illustrate predictions with
SLR or IVR models and to show distinctions between
confidence and prediction intervals.
}
\details{
This function produces a fitted line plot with both
confidence and prediction bands shown.  It then constructs
vertical bars representing the predicted values with the
corresponding interval (chosen with \code{interval}) for
all observations found in \code{newdata}.

This function is only appropriate for SLR and IVR with a
single quantitative covariate and two or fewer factors.

The \code{predictPlot()} is just a pass-through to
\code{predictionPlot()}.
}
\examples{
require(FSA)  # for Mirex data

data(Mirex)
Mirex$year <- factor(Mirex$year)
lm2a <- lm(mirex~weight*species,data=Mirex)
lm3a <- lm(mirex~weight+species,data=Mirex)
lm4 <- lm(mirex~weight,data=Mirex)
op <- par(mfrow=c(2,2),mar=c(3,3,2,1),mgp=c(2,0.7,0))
newdf <- data.frame(weight=c(2,10),species=c("chinook","coho"),year=c("1977","1992"))
predictionPlot(lm2a,newdf,legend=c(0,0.47))
predictionPlot(lm3a,newdf,legend=c(0,0.47))
predictionPlot(lm4,newdf)
predictionPlot(lm4,newdf,interval="c")
par(op)
}
\seealso{
\code{predict} specifically \code{predict.lm} and
\code{fitPlot} from \pkg{FSA}.
}
\keyword{hplot}
\keyword{models}

