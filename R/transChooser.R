#' @title A dynamic graphic used to select transformations for simple linear models.
#' 
#' @description A dynamic graphic used to select a response transformation for an ANOVA (one- or two-way) model or a response or explanatory transformation for a simple linear (SLR) or indicator variable regression (IVR) model.  The function produces a histogram of residuals and a residual plot, optionally with assumption checking method p-values, that are linked to slider bars that allow the user to choose different values of lambda (the power in a power transformation).  This allows the user to dynamically select a power transformation that is likely to produce a model that meets the model assumptions.
#' 
#' @details This function only works for one- and two-way ANOVAs and simple and indicator variable regressions.  The indicator variable regression must be either one- or two-way.  More complicated models can not be considered with transChooser.
#' 
#' Each graphic consists of a histogram of raw residuals on the left and a residual plot on the right (constructed with \code{residPlot} from the \pkg{FSA} package).  If \code{show.stats=TRUE} then the p-value from the Anderson-Darling normality test (as constructed with \code{\link{adTest}}) will be shown above the histogram.  If an ANOVA model is being explored then the p-value from the the outlier test (as constructed with \code{outlierTest} in the \pkg{car} package) will be shown above the histogram.  If a regression model is being explored then the outlier test results will be shown above the residual plot.  If \code{show.stats=TRUE} and an ANOVA model is being explored then the p-value from a Levene's Test of equality of variances (as constructed from \code{LeveneTest} in the \pkg{car} package) will be shown above the residual plot.
#' 
#' If \code{boxplot=TRUE} then the residual plot for the ANOVA models will be a boxplot of residuals for each group rather than a scatterplot of residuals versus group.  This function does not apply to exploration of regression models.
#' 
#' The \code{shifty} and \code{shiftx} arguments are used to provide a constant value to shift the variable being transformed either left (negative value) or right (positive value) along the respective axis.  These values are useful if the original data contains negative numbers as the power transformations generally require non-negative values.  Note that \code{shiftx} is only used if a regression (SLR or IVR) model is being considered.
#' 
#' @aliases transChooser
#' @param object An \code{lm} object or formula depicting a one-way or two-way ANOVA model or a simple linear or indicator variable regression model.
#' @param shifty A numeric shift value for the transformation of the response variable (see details)
#' @param shiftx A numeric shift value for the transformation of the explanatory variable (see details)
#' @param show.stats A logical indicating if the assumption test p-values should (\code{=TRUE} (default)) be printed on the graphics (see details)
#' @param alpha A numeric used to decide the significance cutoff when choosing the color to print the assumption test p-values.  Only has an effect if \code{show.stats=TRUE}.
#' @param boxplot A logical indicating if the residual plot should be constructed as a boxplot (\code{=TRUE}; default) or as a traditional residual plot (\code{=FALSE}).  Only effective if a one- or two-way ANOVA model is being examined.
#' @param col.hist A string used to depict the color of bars in the histogram.
#' @param \dots Other arguments to the generic function.
#' 
#' @return None.  However, a dynamic graphic is produced.
#' 
#' @note This function is designed to allow \sQuote{newbie} students a method that can be used to interactively choose appropriate transformations for the response or explanatory variables in one-way ANOVA, two-way ANOVA, simple linear regression, and indicator variable regressions.  This function allows students to choose possible transformations based on an intuitive analysis of diagnostic plots, in contrast, to depending on a non-intuitive method such as the Box-Cox method.  While this function can be used for research purposes that was not its intent and that is why it is limited to use with only these four simple models.
#' 
#' @seealso \code{\link[car]{leveneTest}}, \code{\link[car]{outlierTest}}, and \code{\link[car]{ncvTest}} in \pkg{car}; \code{\link{adTest}}; and \code{\link[MASS]{boxcox}} in \pkg{MASS}.
#' 
#' @keywords hplot models dynamic
#' 
#' @examples
#' data(Mirex)
#' Mirex$year <- factor(Mirex$year)
#' 
#'  \dontrun{
#' ## example with one-way ANOVA
#' lm1 <- lm(mirex~year,data=Mirex)
#' transChooser(lm1)
#' 
#' ## example with two-way ANOVA
#' lm2 <- lm(mirex~species*year,data=Mirex)
#' transChooser(lm2)
#' 
#' ## example with SLR
#' lm3 <- lm(mirex~weight,data=Mirex)
#' transChooser(lm3)
#' 
#' ## example with IVR
#' lm4 <- lm(mirex~weight*year,data=Mirex)
#' transChooser(lm4)
#' }
#' 
#' @rdname transChooser
#' @export transChooser
transChooser <- function(object,shifty=0,shiftx=0,show.stats=TRUE,
                         boxplot=TRUE,alpha=0.05,col.hist="gray90",...) {
  object <- iTypeoflm(object)
  if (object$type %in% c("SLR","IVR")) transChooser_REGRESS(object,shifty=shifty,shiftx=shiftx,show.stats=show.stats,alpha=alpha,col.hist=col.hist,...)
  else if (object$type %in% c("ONEWAY","TWOWAY")) transChooser_ANOVA(object,shifty=shifty,show.stats=show.stats,boxplot=boxplot,alpha=alpha,col.hist=col.hist,...)
  else if (object$type=="MLR") stop("Multiple linear regression objects are not supported by transChooser.",call.=FALSE)
  else stop("The object supplied in 'object' is not supported by transChooser.",call.=FALSE)
}

transChooser_ANOVA <- function(object,shifty=0,show.stats=TRUE,
                               boxplot=TRUE,alpha=0.05,col.hist="gray90",...) {
  refresh <- function(...) {
    assumPlot_ANOVA(object,lambda=relax::slider(no=1),
                    shifty=shifty,show.stats=show.stats,boxplot=boxplot,
                    alpha=alpha,col.hist=col.hist,...)
  } # end refresh internal function
  
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE,hscale=2,
                   title="ANOVA Power Transformation Chooser",
                   sl.names= c("lambda"),sl.mins=c(-1.0),sl.maxs=c(1.0),
                   sl.deltas=c(0.05),sl.defaults=c(1.0),pos.of.panel="left")
  }
}

transChooser_REGRESS <- function(object,shifty=0,shiftx=0,
                                 show.stats=TRUE,alpha=0.05,col.hist="gray90",...) {  
  refresh <- function(...) {
    assumPlot_REGRESS(object,lambday=relax::slider(no=1),lambdax=relax::slider(no=2),
                      shifty=shifty,shiftx=shiftx,show.stats=show.stats,
                      alpha=alpha,col.hist=col.hist,...)
  }  # end refresh internal function
  
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE,hscale=2,
                   title="Regression Power Transformation Chooser",
                   sl.names=c("lambday","lambdax"),
                   sl.mins=c(-1.0,-1.0),sl.maxs=c(1.0,1.0),
                   sl.deltas=c(0.05,0.05),sl.defaults=c(1.0,1.0),
                   pos.of.panel="left")
  }
}


################################################################################
# Internal functions used in transChooser().
################################################################################
assumPlot_ANOVA <- function(object,lambda,shifty,show.stats,boxplot,alpha,col.hist,...) {
  lambda <- round(lambda,2)
  if (lambda == 0) { 
    y <- log(object$mf[,1]+shifty)
    lbl <- "Residuals from log(Y)"
  } else if (lambda == 1) {
    y <- object$mf[,1]+shifty
    lbl <- "Residuals from Original Y"
  } else {
    y <- (object$mf[,1]+shifty)^lambda
    lbl <- paste0("Residuals from Y^(",formatC(lambda,format="f",digits=2),")") 
  }
  # below controls for whether it is a one-way or two-way ANOVA
  ifelse(any(class(object)=="ONEWAY"), gf <- object$mf[,2], gf <- object$mf[,2]:object$mf[,3])
  lm1 <- stats::lm(y~gf)
  old.par <- graphics::par(mar=c(3.5,3.5,2,1), mgp=c(2,0.75,0), mfcol=c(1,2))
  on.exit(graphics::par(old.par))
  graphics::hist(lm1$residuals,main="",xlab=lbl,yaxt="n",ylab="",col=col.hist)
  if (show.stats) {
    lblADTest(lm1,alpha)
    lblOutTest(lm1,alpha)
  }
  FSA::residPlot(lm1,student=FALSE,outlier.test=TRUE,bp=boxplot,main="",ylab=lbl,inclHist=FALSE)
  if (show.stats) lblLevTest(lm1,alpha)
} ## end internal assumPlot_ANOVA

assumPlot_REGRESS <- function(object,lambday,lambdax,shifty,shiftx,
                              show.stats,alpha,col.hist,...) {
  lambday <- round(lambday,2)
  if (lambday == 0) {
    y <- log(object$mf[,1]+shifty)
    ylbl <- "log(Y)"
  } else if (lambday == 1) {
    y <- object$mf[,1]+shifty
    ylbl <- "Original Y"
  } else { 
    y <- (object$mf[,1]+shifty)^lambday
    ylbl <- paste0("Y^(",formatC(lambday,format="f",digits=2),")")
  }
  lambdax <- round(lambdax,2)
  if (lambdax == 0) {
    x <- log(object$mf[,2]+shiftx)
    xlbl <- "log(X)"
  } else if (lambdax == 1) {
    x <- object$mf[,2]+shiftx
    xlbl <- "Original X"
  } else { 
    x <- (object$mf[,2]+shiftx)^lambdax
    xlbl <- paste0("X^(",formatC(lambdax,format="f",digits=2),")") 
  }
  # Below handles differences between SLR and IVR
  if (!any(class(object)=="IVR")) lm1 <- stats::lm(y~x)
  # Below determines if it is a one-way or a two-way IVR and adjusts accordingly
  else if (dim(object$mf)[2]==3) lm1 <- stats::lm(y~x*object$mf[,3])
  else if (dim(object$mf)[2]==4) lm1 <- stats::lm(y~x*object$mf[,3]*object$mf[,4])
  else stop("Function only works with IVR models with 1 or 2 factors.",call.=FALSE)
  old.par <- graphics::par(mar=c(3.5,3.5,3,1), mgp=c(2,0.5,0), mfcol=c(1,2))
  graphics::hist(lm1$residuals,main="",xlab=paste0("Residuals from ",ylbl,"~",xlbl),
                 yaxt="n",ylab="",col=col.hist)
  if (show.stats) lblADTest(lm1,alpha,line=0.5)
  FSA::residPlot(lm1,main="",xlab=paste0("Fitted Values from ",ylbl,"~",xlbl),
                 ylab=paste0("Residuals from ",ylbl,"~",xlbl),inclHist=FALSE)
  if (show.stats) lblOutTest(lm1,alpha,line=0.5)
  on.exit(graphics::par(old.par))
} ## end internal assumPlot_REGRESS

lblADTest <- function(lmobj,alpha,line=1) {
  ad.p <- formatC(adTest(lmobj$residuals)$p.value,digits=4,format="f")
  graphics::mtext(paste("Anderson-Darling p-value=",ad.p),
                  line=line,col=ifelse(ad.p<alpha,"red","black"))
}

lblOutTest <- function(lmobj,alpha,line=0) {
  out.p <- car::outlierTest(lmobj)$bonf.p
  if (length(out.p)>1) { out.p <- min(out.p) }
  if (is.na(out.p) | out.p>1) { out.p <- 1 }
  out.p <- formatC(out.p,digits=4,format="f")
  graphics::mtext(paste("Outlier test p-value=",out.p),
                  line=line,col=ifelse(out.p<alpha,"red","black"))
}

lblLevTest <- function(lmobj,alpha,line=0.5) {
  lev.p <- formatC(car::leveneTest(lmobj)[1,3],digits=4,format="f")
  graphics::mtext(paste("Levene's test p-value=",lev.p),line=line,
                  col=ifelse(lev.p<alpha,"red","black"))
}

lblNCVTest <- function(lmobj,alpha,line=0.5) {
  ncv.p <- formatC(car::ncvTest(lmobj)$p,digits=4,format="f")
  graphics::mtext(paste("NCV test p-value=",ncv.p),line=line,
                  col=ifelse(ncv.p<alpha,"red","black"))
}
