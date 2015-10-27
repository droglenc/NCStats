#' Shows the predicted value and interval on a fitted line plot.
#' 
#' Shows the predicted value and interval on a fitted line plot.  This function is used to illustrate predictions with SLR or IVR models and to show distinctions between confidence and prediction intervals.
#' 
#' This function produces a fitted line plot with both confidence and prediction bands shown.  It then constructs vertical bars representing the predicted values with the corresponding interval (chosen with \code{interval}) for all observations found in \code{newdata}.
#' 
#' This function is only appropriate for SLR and IVR with a single quantitative covariate and two or fewer factors.
#' 
#' The \code{predictPlot()} is just a pass-through to \code{predictionPlot()}.
#' 
#' @aliases predictionPlot predictPlot
#' 
#' @param mdl an \code{lm} or \code{nls} object (i.e., returned from fitting a model with either \code{lm} or \code{nls})
#' @param newdata A data frame in which to look for variables with which to predict.  This cannot be omitted as it is with \code{predict}
#' @param interval a string indicating whether to plot confidence (\code{="confidence"}) or prediction (\code{="prediction"}) intervals
#' @param conf.level a decimal numeric indicating the level of confidence to use for confidence and prediction intervals (default is \code{0.95})
#' @param lty a numeric indicating the type of line used for representing the intervals (see \code{par})
#' @param lwd a numeric indicating the width of line used for representing the intervals (see \code{par})
#' @param \dots Other arguments to the \code{fitPlot} function.  For example, include \code{legend=TRUE} to include a legend on the fitted line plot for an IVR.
#' 
#' @return A data.frame is returned that contains the number of the new observation (for comparison to the graphic that is produced), the values of the variables in \code{newdata}, and the predicted values at those observed values.
#' 
#' @seealso \code{predict} specifically \code{predict.lm} and \code{fitPlot} from \pkg{FSA}.
#' 
#' @keywords hplot models
#' 
#' @examples
#' require(FSA)  # for Mirex data
#' 
#' data(Mirex)
#' Mirex$year <- factor(Mirex$year)
#' lm2a <- lm(mirex~weight*species,data=Mirex)
#' lm3a <- lm(mirex~weight+species,data=Mirex)
#' lm4 <- lm(mirex~weight,data=Mirex)
#' op <- par(mfrow=c(2,2),mar=c(3,3,2,1),mgp=c(2,0.7,0))
#' newdf <- data.frame(weight=c(2,10),species=c("chinook","coho"),year=c("1977","1992"))
#' predictionPlot(lm2a,newdf,legend=c(0,0.47))
#' predictionPlot(lm3a,newdf,legend=c(0,0.47))
#' predictionPlot(lm4,newdf)
#' predictionPlot(lm4,newdf,interval="c")
#' par(op)
#' 
#' @rdname predictionPlot
#' @export predictPlot
predictPlot <- function(...) {
  predictionPlot(...)
}

#' @rdname predictionPlot
#' @export predictionPlot
predictionPlot <- function(mdl,newdata,interval="prediction",conf.level=0.95,lty=1,lwd=3,...) {
  lmtype <- iTypeoflm(mdl)
  if (lmtype$type!="SLR" & lmtype$type!="IVR") stop("\n  Function only works for SLR or IVR models.",call.=FALSE)
  if (dim(lmtype$mf)[2]>4) stop("\n  Function only works for IVR models with one covariate and two or fewer factors.",call.=FALSE)
  if (missing(newdata)) stop("\n newdata argument is missing.",call.=FALSE)
  if (dim(newdata)[1]<1 | dim(newdata)[2]<1) stop("\n newdata is empty",call.=FALSE)
  if (!all(names(lmtype$mf)[-1] %in% names(newdata))) stop("\n Not all variables found in mdl are in newdata.",call.=FALSE)
    
  FSA::fitPlot(mdl,interval=interval,cex.main=0.8,conf.level=conf.level,...)
  preds <- stats::predict(mdl,newdata,interval=interval,conf.level=conf.level)
  for (i in 1:dim(newdata)[1]) {
    graphics::points(rep(newdata[i,1],3),preds[i,],col="seagreen4",pch=95,lwd=lwd,xpd=NA,cex=2)
    graphics::lines(rep(newdata[i,1],2),preds[i,2:3],col="seagreen4",lty=lty,lwd=lwd,xpd=NA)
    graphics::text(newdata[i,1],preds[i,1],i,pos=4,col="seagreen4",cex=1.25)
  }
  df <- data.frame(1:dim(newdata)[1],newdata,preds)
  names(df) <- c("obs",names(newdata),colnames(preds))
  df
}
