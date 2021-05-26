#' @title Graphics for assessing assumptions of simple linear models.
#' 
#' @description A histogram of residuals and a boxplot of residuals by \dQuote{groups} for ANOVA tests or a residual plot for regression tests is produced. Optionally p-values from the Anderson-Darling test of normality, the outlier test, and Levene's Test for equal variances is shown. The user may also iteratively try power transformations for the response and explanatory variable through arguments in \code{assumptionCheck} or a dynamic graphic using slider bars in \code{transChooser}.
#' 
#' @details These functions only work for one- and two-way ANOVAs and simple and one- or two-way indicator variable regressions.
#' 
#' Each graphic consists of a histogram of raw residuals on the left and a residual plot (constructed with \code{residPlot} from \pkg{FSA} package) or a boxplot of residuals by group if \code{boxplot=TRUE} in \code{assumptionCheck} or the boxplot check box is selected in the gear box when using \code{transChooser}. P-values from assumption tests will be shown if \code{show.stats=TRUE} in \code{assumptionCheck} or if a check box is selected in the gear box when using \code{transChooser}. The Anderson-Darling p-values is from \code{\link{adTest}}), the outlier test p-value is from \code{\link[car]{outlierTest}} in the \pkg{car} package), and the Levene's Test p-value is from \code{\link[car]{leveneTest}} in the \pkg{car} package).
#' The \code{lambday} and \code{lambdx} arguments in \code{assumptionCheck} or the slider bar values in the gear box when using \code{transChooser} are values for the power transformation of the response and explanatory variables, respectively. Note that a lambda of 0 corresponds to a natural log transformation. Note that \code{lambdax} is only used if a regression (SLR or IVR) model is being considered.
#' 
#' The \code{shifty} and \code{shiftx} arguments are used to provide a constant value to shift the variable being transformed either left (negative value) or right (positive value) along the respective axis. These values are useful if the original data contains negative numbers as the power transformations generally require non-negative values. Note that \code{shiftx} is only used if a regression (SLR or IVR) model is being considered.
#' 
#' @aliases transChooser
#' @param object An \code{lm} object or formula depicting a one-way or two-way ANOVA model or a simple linear or indicator variable regression model.
#' @param lambday A numeric value for the power of the transformation of the response variable (see details).
#' @param lambdax A numeric value for the power of the transformation of the explanatory variable (see details).
#' @param shifty A numeric shift value for the transformation of the response variable (see details).
#' @param shiftx A numeric shift value for the transformation of the explanatory variable (see details).
#' @param show.stats A logical indicating if the assumption test p-values should (\code{=TRUE} (default)) be printed on the graphics (see details).
#' @param alpha A numeric used to decide the significance cutoff when choosing the color to print the assumption test p-values. Only has an effect if \code{show.stats=TRUE}.
#' @param boxplot A logical indicating if the residual plot should be constructed as a boxplot (\code{=TRUE}) or as a traditional residual plot (\code{=FALSE}). Only effective if a one- or two-way ANOVA model is being examined.
#' @param col.hist A string used to depict the color of bars in the histogram.
#' 
#' @return None. However, a graph, static for \code{assumptionCheck} and dynamic for \code{transChooser}, is produced.
#' 
#' @note This function is designed to allow \sQuote{newbie} students a method that can be used to quickly test assumptions for simple linear models or to interactively choose appropriate transformations for the response or explanatory variables in these models. This function allows students to choose possible transformations based on an intuitive analysis of diagnostic plots, in contrast, to depending on a non-intuitive method such as the Box-Cox method. While this function can be used for research purposes that was not its intent and that is why it is limited to use with only these four simple models.
#' 
#' @seealso \code{\link[car]{leveneTest}} and \code{\link[car]{outlierTest}}; \code{\link{adTest}}; and \code{\link[MASS]{boxcox}} in \pkg{MASS}.
#' 
#' @keywords hplot models dynamic
#' 
#' @examples
#' if (require(FSA)) {
#' data(Mirex)
#' Mirex$year <- factor(Mirex$year)
#' Mirex$cyear <- as.character(Mirex$year)
#' 
#' aov1 <- lm(mirex~year,data=Mirex)
#' assumptionCheck(aov1)
#' assumptionCheck(aov1,lambday=0)
#' 
#' aov1c <- lm(mirex~cyear,data=Mirex)
#' assumptionCheck(aov1c)
#' assumptionCheck(aov1c,lambday=0)
#' 
#' aov2 <- lm(mirex~species*year,data=Mirex)
#' assumptionCheck(aov2)
#' 
#' slr1 <- lm(mirex~weight,data=Mirex)
#' assumptionCheck(slr1)
#' assumptionCheck(slr1,lambday=0)
#' assumptionCheck(slr1,lambdax=0)
#' 
#' ivr1 <- lm(mirex~weight*year,data=Mirex)
#' assumptionCheck(ivr1)
#' }
#' 
#' \dontrun{
#' # Demonstrates interactive transChooser function
#' transChooser(aov1)
#' transChooser(aov2)
#' transChooser(slr1)
#' transChooser(ivr1)
#' }
#' 
#' @rdname assumptionCheck
#' @export assumptionCheck
assumptionCheck <- function(object,lambday=1,lambdax=1,shifty=0,shiftx=0,
                            show.stats=TRUE,boxplot=TRUE,alpha=0.05,
                            col.hist="gray90") {
  ## turn off warnings
  suppressWarnings({
    ## Get the linear model type of object
    if (!"lm" %in% class(object)) stop("'object' must be from 'lm'.",call.=FALSE)
    object <- iTypeoflm(object)
    if (!object$type %in%  c("SLR","IVR","ONEWAY","TWOWAY"))
      stop("'object' must be an SLR, IVR, or ANOVA",call.=FALSE)
    ## Choose method based on type of model
    if (object$type %in% c("SLR","IVR")) {
      iAssumPlot_REGRESS(object,lambday,lambdax,shifty,shiftx,show.stats,alpha,col.hist)
    }
    else {
      iAssumPlot_ANOVA(object,lambday,shifty,show.stats,boxplot,alpha,col.hist)
    }
  })
}

#' @export transChooser
transChooser <- function(object,shifty=0,shiftx=0,show.stats=TRUE,
                         boxplot=TRUE,alpha=0.05,col.hist="gray90") {
  ## turn off warnings
  suppressWarnings({
    ## Get the linear model type of object
    if (!"lm" %in% class(object)) stop("'object' must be from 'lm'.",call.=FALSE)
    object <- iTypeoflm(object)
    if (!object$type %in%  c("SLR","IVR","ONEWAY","TWOWAY"))
      stop("'object' must be an SLR, IVR, or ANOVA",call.=FALSE)
    ## See if RStudio can be used
    if (iCheckRStudio()) {
      if (iChk4Namespace("manipulate")) {
        if (object$type %in% c("SLR","IVR")) iTC_REGRESS1(object,shifty,shiftx,
                                                          show.stats,alpha,col.hist)
        else iTC_ANOVA1(object,shifty,show.stats,boxplot,alpha,col.hist)
      }
    }
  })
}


################################################################################
## Internal functions used by assumptionCheck().
################################################################################
iAssumPlot_REGRESS <- function(object,lambday,lambdax,shifty,shiftx,
                               show.stats,alpha,col.hist) {
  if (lambday == 0) {
    y <- log(object$mf[,object$Rpos]+shifty)
    ylbl <- "log(Y)"
  } else if (lambday == 1) {
    y <- object$mf[,object$Rpos]+shifty
    ylbl <- "Original Y"
  } else { 
    y <- (object$mf[,object$Rpos]+shifty)^lambday
    ylbl <- paste0("Y^(",formatC(lambday,format="f",digits=2),")")
  }
  if (lambdax == 0) {
    x <- log(object$mf[,object$ENumPos]+shiftx)
    xlbl <- "log(X)"
  } else if (lambdax == 1) {
    x <- object$mf[,object$ENumPos]+shiftx
    xlbl <- "Original X"
  } else { 
    x <- (object$mf[,object$ENumPos]+shiftx)^lambdax
    xlbl <- paste0("X^(",formatC(lambdax,format="f",digits=2),")") 
  }
  ## Handle differences between SLR and IVR, and adjust if it
  ##   is one- or two way IVR)
  if (object$type=="SLR") lm1 <- stats::lm(y~x)
  else if (object$Enum==2) lm1 <- stats::lm(y~x*object$mf[,object$EFactPos[1]])
  else if (object$Enum==3) lm1 <- stats::lm(y~x*object$mf[,object$EFactPos[1]]*object$mf[,object$EFactPos[2]])
  else stop("Only works with IVR models with 1 or 2 factors.",call.=FALSE)
  withr::local_par(list(mar=c(3.5,3.5,3,1),mgp=c(2,0.5,0),mfcol=c(1,2)))
  graphics::hist(lm1$residuals,main="",xlab=paste0("Residuals from ",ylbl,"~",xlbl),
                 yaxt="n",ylab="",col=col.hist)
  if (show.stats) iLblADTest(lm1,alpha,line=0.5)
  if (object$type=="SLR") {
    FSA::residPlot(lm1,main="",xlab=paste0("Fitted Values from ",ylbl,"~",xlbl),
                   ylab=paste0("Residuals from ",ylbl,"~",xlbl),inclHist=FALSE,
                   col=FSA::col2rgbt("black",2/3))
  } else {
    FSA::residPlot(lm1,main="",xlab=paste0("Fitted Values from ",ylbl,"~",xlbl),
                   ylab=paste0("Residuals from ",ylbl,"~",xlbl),inclHist=FALSE,
                   col=FSA::col2rgbt("black",2/3),legend=FALSE)
  }
  if (show.stats) iLblOutlierTest(lm1,alpha,line=0.5)
} ## end internal iAssumPlot_REGRESS

iAssumPlot_ANOVA <- function(object,lambda,shifty,show.stats,boxplot,alpha,col.hist) {
  lambda <- round(lambda,2)
  if (lambda == 0) { 
    y <- log(object$mf[,object$Rpos]+shifty)
    lbl <- "Residuals from log(Y)"
  } else if (lambda == 1) {
    y <- object$mf[,object$Rpos]+shifty
    lbl <- "Residuals from Original Y"
  } else {
    y <- (object$mf[,object$Rpos]+shifty)^lambda
    lbl <- paste0("Residuals from Y^(",formatC(lambda,format="f",digits=2),")") 
  }
  # below controls for whether it is a one-way or two-way ANOVA
  gf <- object$mf[,object$EFactPos]
  if (inherits(object,"TWOWAY")) gf <- interaction(object$mf[,object$EFactPos])
  lm1 <- stats::lm(y~gf)
  withr::local_par(list(mar=c(3.5,3.5,2,1),mgp=c(2,0.75,0),mfcol=c(1,2)))
  graphics::hist(lm1$residuals,main="",xlab=lbl,yaxt="n",ylab="",col=col.hist)
  if (show.stats) {
    iLblADTest(lm1,alpha)
    iLblOutlierTest(lm1,alpha)
  }
  FSA::residPlot(lm1,outlier.test=TRUE,bp=boxplot,main="",ylab=lbl,
                 inclHist=FALSE,col=FSA::col2rgbt("black",2/3))
  if (show.stats) iLblLevenesTest(lm1,alpha)
} ## end internal iAssumPlot_ANOVA

iLblADTest <- function(lmobj,alpha,line=1) {
  ad.p <- formatC(adTest(lmobj$residuals)$p.value,digits=4,format="f")
  graphics::mtext(paste("Anderson-Darling p-value=",ad.p),
                  line=line,col=ifelse(ad.p<alpha,"red","black"))
}

iLblOutlierTest <- function(lmobj,alpha,line=0) {
  out.p <- car::outlierTest(lmobj)$bonf.p
  if (length(out.p)>1) { out.p <- min(out.p) }
  if (is.na(out.p) | out.p>1) { out.p <- 1 }
  out.p <- formatC(out.p,digits=4,format="f")
  graphics::mtext(paste("Outlier test p-value=",out.p),
                  line=line,col=ifelse(out.p<alpha,"red","black"))
}

iLblLevenesTest <- function(lmobj,alpha,line=0.5) {
  lev.p <- formatC(car::leveneTest(lmobj)[1,3],digits=4,format="f")
  graphics::mtext(paste("Levene's test p-value=",lev.p),line=line,
                  col=ifelse(lev.p<alpha,"red","black"))
}

iLblNCVTest <- function(lmobj,alpha,line=0.5) {
  ncv.p <- formatC(car::ncvTest(lmobj)$p,digits=4,format="f")
  graphics::mtext(paste("NCV test p-value=",ncv.p),line=line,
                  col=ifelse(ncv.p<alpha,"red","black"))
}


##############################################################
## Internal functions that use the manipulate package       ##
##############################################################
iTC_REGRESS1 <- function(object,shifty,shiftx,show.stats,alpha,col.hist) {
  ## Trying to fix "no visible bindings" problem for Check
  lambdax <- lambday <- NULL
  manipulate::manipulate(
    {iAssumPlot_REGRESS(object,lambday,lambdax,shifty,shiftx,show.stats,alpha,col.hist)},
    lambday=manipulate::picker(1,0.5,0.333,0.25,0,-0.25,-0.33,-0.5,-1,label="Y Lambda"),
    lambdax=manipulate::picker(1,0.5,0.333,0.25,0,-0.25,-0.33,-0.5,-1,label="X Lambda"),
    show.stats=manipulate::checkbox(TRUE,label="Show Test Results")
  ) # end manipulate
}

iTC_ANOVA1 <- function(object,shifty,show.stats,boxplot,alpha,col.hist) {
  ## Trying to fix "no visible bindings" problem for Check
  lambday <- NULL
  manipulate::manipulate(
    {iAssumPlot_ANOVA(object,lambday,shifty,show.stats,boxplot=TRUE,alpha,col.hist)},
    lambday=manipulate::picker(1,0.5,0.333,0.25,0,-0.25,-0.33,-0.5,-1,label="Lambda"),
    show.stats=manipulate::checkbox(TRUE,label="Show Test Results"),
    boxplot=manipulate::checkbox(TRUE,label="Use Boxplot for Residuals")
  ) # end manipulate
}
