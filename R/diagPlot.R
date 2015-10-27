#' @title Constructs plots of diagnostic measures for linear models.
#' 
#' @description Used to construct plots of diagnostic measures for linear models.  Also used to identify \dQuote{extreme} values of diagnostic measures for a linear model.
#' 
#' @details This function produces a graphic that consists of at most six separate plots --
#'   \enumerate{
#'     \item Studentized residuals versus leverages,
#'     \item COVRATIO-1 versus fitted values,
#'     \item Cook's Distance versus fitted values,
#'     \item DFFits versus fitted values,
#'     \item DFBetas for slope versus DFBetas for intercept, and
#'     \item fitted line plot.
#'   }
#' 
#' Each separate graph may have various individuals marked with their observation number.  Observation numbers in red are the most extreme value that exceeds the cutoff value for the diagnostic measure plotted on that particular graph.  Observation numbers in blue are observations that exceeded a cutoff value for at least one of the diagnostic measures NOT plotted on that particular graph.  Thus, observations marked in red are \dQuote{unusual} observations for the diagnostic measure shown on the plot whereas observations marked in blue are \dQuote{unusual} observations for some other diagnostic measure but not for the diagnostic measure shown on the plot.  The fitted line plot has all \dQuote{unusual} observations marked with separate colors and the fitted line with that observation removed shown in the same color.
#' 
#' If more than one observation has the same extreme value for one of the diagnostics then only the first individual with the value is returned.
#' 
#' If the linear model object is other than a simple linear regression then only the first four plots are constructed.
#' 
#' Diagnostic statistic values are computed with the \code{rstudent} and \code{\link[stats]{influence.measures}} functions.
#' 
#' @param mdl an \code{lm} object (i.e., returned from fitting a model with \code{lm}).
#' 
#' @return In addition to the graphic described in the details, a vector containing the row numbers of observations that were flagged as unusual by one of the diagnostic statistics.  This vector can be assigned to an object and used to modify plots or easily remove the individuals from the data.frame.
#' 
#' @note This function is meant to allow newbie students the ability to easily construct plots for diagnosing \dQuote{problem individuals} for one-way ANOVA, two-way ANOVA, simple linear regression, and indicator variable regressions.  The plots can generally be constructed simply by submitting a saved linear model to this function.  This function thus allows newbie students to interact with and visualize moderately complex linear models in a fairly easy and efficient manner.
#' 
#' @seealso \code{\link[FSA]{fitPlot}} and \code{\link[FSA]{residPlot}} from \pkg{FSA}; \code{\link{highlight}}; \code{\link[stats]{influence.measures}}; and \code{\link[car]{outlierTest}} and \code{\link[car]{influence.plot}} in \pkg{car}.
#' 
#' @keywords hplot models
#' 
#' @examples
#' lm1 <- lm(Sepal.Length~Petal.Length*Species,data=iris)
#' diagPlot(lm1)
#' ## Simple linear regression
#' lm2 <- lm(Sepal.Length~Petal.Length,data=iris)
#' # Produces plot and saves flagged observations
#' pts <- diagPlot(lm2)
#' if (require(FSA)) {
#'   # Constructs a fitted line plot
#'   fitPlot(lm2)
#'   # Highlights flagged observations on fitted-line plot
#'   highlight(Sepal.Length~Petal.Length,data=iris,pts=pts)
#' }
#' 
#' ## Example showing outlier detection
#' df <- data.frame(x=runif(100),y=c(7,runif(99)))
#' lma <- lm(y~x,data=df)
#' diagPlot(lma)
#' 
#' @export
diagPlot <- function(mdl) {
  lmtype <- iTypeoflm(mdl)
  if (lmtype$type=="SLR") { 
    old.par <- graphics::par(mfrow=c(3,2),mar=c(3,3,1,1),mgp=c(2,0.75,0),xpd=TRUE)
  } else { 
    old.par <- graphics::par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,0.75,0),xpd=TRUE)
  } 
  on.exit(graphics::par(old.par))
  res <- iSigDiag(mdl)
  inf <- stats::influence.measures(mdl)
  all.pts <- unique(res$obs[res$inf])
  
  y <- stats::rstudent(mdl)
  x <- inf$infmat[,"hat"]
  graphics::plot(y~x,xlab="Leverages (Hats)",ylab="Studentized Residuals",pch=19)
  all.pts1 <- all.pts[-which(all.pts==res$obs[1])]
  all.pts1 <- all.pts1[-which(all.pts1==res$obs[2])]
  if (length(all.pts1)>=1) graphics::text(x[all.pts1],y[all.pts1],all.pts1,col="blue")
  if (res$inf[1]) graphics::text(x[res$obs[1]],y[res$obs[1]],res$obs[1],col="red",cex=1.5)
  if (res$inf[2]) graphics::text(x[res$obs[2]],y[res$obs[2]],res$obs[2],col="red",cex=1.5)
  
  y <- inf$infmat[,"cov.r"]-1
  x <- mdl$fitted.values
  graphics::plot(y~x,ylab="COVRATIO-1",xlab="Fitted Values",pch=19)
  all.pts1 <- all.pts[-which(all.pts==res$obs[3])]
  if (length(all.pts1)>=1) graphics::text(x[all.pts1],y[all.pts1],all.pts1,col="blue")
  if (res$inf[3]) graphics::text(x[res$obs[3]],y[res$obs[3]],res$obs[3],col="red",cex=1.5)  
  
  y <- inf$infmat[,"cook.d"]
  graphics::plot(y~x,ylab="Cook's Distances",xlab="Fitted Values",pch=19)
  all.pts1 <- all.pts[-which(all.pts==res$obs[4])]
  if (length(all.pts1)>=1) graphics::text(x[all.pts1],y[all.pts1],all.pts1,col="blue")
  if (res$inf[4]) graphics::text(x[res$obs[4]],y[res$obs[4]],res$obs[4],col="red",cex=1.5)    
  
  y <- inf$infmat[,"dffit"]
  graphics::plot(y~x,ylab="DFfit",xlab="Fitted Values",pch=19)
  all.pts1 <- all.pts[-which(all.pts==res$obs[5])]
  if (length(all.pts1)>=1) graphics::text(x[all.pts1],y[all.pts1],all.pts1,col="blue")
  if (res$inf[5]) graphics::text(x[res$obs[5]],y[res$obs[5]],res$obs[5],col="red",cex=1.5)   
  
  if (lmtype$type=="SLR") {
    y <- inf$infmat[,2]
    x <- inf$infmat[,1]
    graphics::plot(y~x,ylab="Slope DFBetas",xlab="Intercept DFBetas",pch=19)
    all.pts1 <- all.pts[-which(all.pts==res$obs[6])]
    all.pts1 <- all.pts1[-which(all.pts1==res$obs[7])]
    if (length(all.pts1)>=1) graphics::text(x[all.pts1],y[all.pts1],all.pts1,col="blue")
    if (res$inf[6]) graphics::text(x[res$obs[6]],y[res$obs[6]],res$obs[6],col="red",cex=1.5)
    if (res$inf[7]) graphics::text(x[res$obs[7]],y[res$obs[7]],res$obs[7],col="red",cex=1.5)
    
    if (iChk4Namespace("FSA")) {
      FSA::fitPlot(mdl,main="",pch=19)
      if (length(all.pts)>=1) {
        clrs <- c("blue","red","cyan","darkgreen","magenta","pink","orange")
        for (i in 1:length(all.pts)) { 
          lm2 <- stats::lm(lmtype$mf[-all.pts[i],1]~lmtype$mf[-all.pts[i],2])
          graphics::abline(lm2,col=clrs[i],xpd=FALSE)
          graphics::text(lmtype$mf[all.pts[i],2],lmtype$mf[all.pts[i],1],all.pts[i],
                         col=clrs[i],cex=1.5)
        } # end i
      } # end if      
    }
  }
  cat("\nUnusual Observation Results\n")
  print(res)
  cat("\nData for Highlighted Individuals\n")
  print(data.frame(obs=sort(all.pts),lmtype$mf[sort(all.pts),]))
  cat("\n")
  invisible(all.pts)
}



iSigDiag <- function(mdl) {
  k <- dim(mdl$model)[2]-1
  n <- dim(mdl$model)[1]
  cat(k,"predictors and",n,"individuals\n")
  inf <- stats::influence.measures(mdl)
  meas <- c("|Studentized Residuals|","Leverage","|COVRATIO-1|","Cook's Distance","|DFFits|")
  val <- cutoff <- obs <- sig <- NULL
  # leverages (last [1] corrects for two individuals with same value)
  val <- c(val,max(inf$infmat[,"hat"]))
  obs <- c(obs,which(inf$infmat[,"hat"]==val[1])[1])
  # Covariance Ratio
  val <- c(val,max(abs(inf$infmat[,"cov.r"]-1)))
  obs <- c(obs,which(abs(inf$infmat[,"cov.r"]-1)==val[2])[1])
  # Cook's Distance
  val <- c(val,max(inf$infmat[,"cook.d"]))
  obs <- c(obs,which(inf$infmat[,"cook.d"]==val[3])[1])
  # DFFits
  val <- c(val,max(abs(inf$infmat[,"dffit"])))
  obs <- c(obs,which(abs(inf$infmat[,"dffit"])==val[4])[1])
  # cutoff values -- same order
  cutoff <- round(c(3*(k+1)/n,3*(k+1)/n,4/(n-k-1),2*sqrt((k+1)/(n-k-1))),5)
  if (iTypeoflm(mdl)$type=="SLR") {
    meas <- c(meas,"Slope DFBetas","Intercept DFBetas")
    # DFBetas -- slope
    val <- c(val,max(inf$infmat[,2]))
    obs <- c(obs,which(inf$infmat[,2]==val[5])[1])
    # DFBetas -- intercept
    val <- c(val,max(inf$infmat[,1]))
    obs <- c(obs,which(inf$infmat[,1]==val[6])[1])
    cutoff <- round(c(cutoff,2/sqrt(n),2/sqrt(n)),5)
  }
  # significance?
  sig <- val>cutoff
  out <- car::outlierTest(mdl)
  # residuals
  val <- c(out$rstudent,val)
  obs <- c(as.numeric(names(out$bonf.p)),obs)
  cutoff <- c("NA",cutoff)
  # deal with outlierTest
  if (is.na(out$bonf.p)) sig <- c(FALSE,sig)
  else ifelse(out$bonf.p < 0.05,sig <- c(TRUE,sig),sig <- c(FALSE,sig))
  # put it all together
  data.frame(measure=meas,value=round(val,5),cutoff=cutoff,inf=sig,obs=as.numeric(obs))
}