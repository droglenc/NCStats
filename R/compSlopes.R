#'Tests for significant differences among all pairs of slopes in an IVR.
#'
#'Tests for significant differences among all pairs of slopes in an indicator
#'variable regression where the indicator variables all stem from one factor.
#'
#'In an indicator variable regression the coefficient for the interaction
#'between the covariate (x) and an indicator variable tests for a difference in
#'slopes between the level of the indicator variable and the reference level.
#'Thus, all indicator variables from a particular linear model fit only compare
#'slopes with the reference level.  Other slope comparisons can be found by
#'changing the reference level but this requires re-fitting the model.  This
#'function automates this sequential process and produces a data frame that
#'shows the estimated difference, an unadjusted confidence interval for the
#'difference, and the unadjusted p-value for testing that the difference in
#'slopes is equal to zero.  In addition, the function provides an adjusted
#'p-value for testing that the difference in slopes is equal to zero and a
#'statement about whether each null hypothesis is signficant or not.
#'
#'The adjusted p-values can be computed with a wide variety of methods -- FDR,
#'BH, BY, bonferroni, holm, hochberg, and hommel.  This function basically
#'works as a wrapper function that sends the unadjusted \dQuote{raw} p-values
#'to the \code{p.adjust} function in the base R program.  These functions
#'should be consulted for further description of the methods used.
#'
#'@aliases compSlopes print.compSlopes
#'@param mdl a \code{lm} object.
#'@param control a string indicating the method of control to use.  See details.
#'@param alpha a decimal numeric indicating the desired level for the
#'experimentwise error rate
#'@param x A \code{compSlopes} object (i.e., returns from \code{compSlopes}).
#'@param order.slopes A logical indicating whether the slopes should be order
#'from smallest to largest upon output.
#'@param digits A numeric that controls the number of digits to print.
#'@param \dots Other arguments sent to \code{print}.
#'@return A list with three components.  The first component contains the
#'\code{control} method.  The second component is called \code{comparisons} and
#'is a data frame that contains the following components:
#'
#'\tabular{ll}{
#'\code{comparison} \tab Description of how the difference in levels was computed.\cr
#'\code{diff} \tab The estimated difference in slope values.\cr
#'\code{lwr} \tab Lower confidence bound for difference in slope values.\cr 
#'\code{upr} \tab Upper confidence bound for difference in slope values.\cr 
#'\code{raw.p} \tab Unadjusted p-value for testing the difference in slopes is zero.\cr 
#'\code{adj.p} \tab Adjusted p-value for testing the difference in slopes is zero.\cr
#'}
#'
#'The third component of the list is called \code{slopes} and is a data frame
#'that contains the following components:
#'\tabular{ll}{
#'\code{level} \tab A level name.\cr
#'\code{slope} \tab The estimated slope value for the given level.\cr
#'\code{lwr} \tab Lower confidence bound for difference in slope values.\cr
#'\code{upr} \tab Upper confidence bound for difference in slope values.\cr
#'\code{raw.p} \tab Unadjusted p-value for testing the slope is zero.\cr 
#'\code{adj.p} \tab Adjusted p-value for testing the slope is zero.\cr
#'}
#'
#'The \code{print} function prints the results nicely.
#'@note This function only works for models with one factor variable.
#'@seealso \code{fitPlot} in \pkg{FSA}, \code{p.adjust}.
#'@keywords htest
#'@examples
#'require(FSA)      # for fitPlot and Mirex data
#'
#'data(Mirex)
#'Mirex$year <- factor(Mirex$year)
#'# fit an indicator variable regression
#'lm1 <- lm(mirex~weight*year,data=Mirex)
#'# visualize the results
#'fitPlot(lm1)
#'# compare all pairs of slopes using an FDR control
#'compSlopes(lm1)
#'# compare all pairs of slopes using the Holm method of control
#'compSlopes(lm1,control="holm")
#'
#'@rdname compSlopes
#'@export
compSlopes <- function(mdl,control=c("fdr","BH","BY","bonferroni","holm","hochberg","hommel"),alpha=0.05,order.slopes=TRUE,digits=getOption("digits")) {
  ## Internal functions
    extract.slopes <- function(sum,conf,isdf,i,conf.level) {
      isdf$slopes[i] <- sum$coefficients[2,1]
      isdf$raw.p[i] <- sum$coefficients[2,4]
      isdf$lwr[i] <- conf[2,1]
      isdf$upr[i] <- conf[2,2]
      isdf[2:5] <- round(isdf[2:5],5)
      invisible(isdf)
    } # end extract.slopes
                          
    extract.comparisons <- function(sum,conf,icdf,i,lev.names,lev.num,conf.level) {
      coeffs <- sum$coefficients[,1]
      pvals <- sum$coefficients[,4]
      comps <- (lev.num-1):1
      pos <- ifelse(i==1,1,1+sum(comps[1:(i-1)]))                               # find position to start replacements
      for (j in (i+1):lev.num) {                                                # info on comparison of slopes w/ ref group
        icdf$comparison[pos] <- paste(lev.names[j],"-",lev.names[i],sep="")
        icdf$diff[pos] <- coeffs[(length(pvals)-lev.num+j)]
        icdf$raw.p[pos] <- pvals[(length(coeffs)-lev.num+j)]
        icdf$lwr[pos] <- conf[(length(pvals)-lev.num+j),1]
        icdf$upr[pos] <- conf[(length(pvals)-lev.num+j),2]
        icdf[2:5] <- round(icdf[2:5],5)
        pos <- pos+1
      } # for j    
      invisible(icdf)
    } # end extract.comparisons

    perform.control <- function(control,idf) {
      res <- p.adjust(idf[,"raw.p"],control)
      idf <- data.frame(idf,adj.p=round(res,5))
      invisible(idf)
    }
  
  # start compSlopes main function
  control <- match.arg(control)
  conf.level <- 1-alpha                                                         # set confidence level from significance level  
  lmtype <- FSA:::typeoflm(mdl)
  if (lmtype$type!="IVR") stop("\n  Function only works for an indicator variable regression.",call.=FALSE)
  y <- mdl$model[,1]                                                            # isolate model information
  x <- mdl$model[,2]
  g <- mdl$model[,3]                               
  lev.names <- levels(g)
  lev.num <- length(levels(g))
  sdf <- data.frame(level=lev.names,slopes=rep(0,lev.num),lwr=rep(0,lev.num),
                    upr=rep(0,lev.num),raw.p=rep(0,lev.num))                    # initiate slopes info data.frame
  num.comps <- choose(lev.num,2)                                                # compute total number of comparisons
  cdf <- data.frame(comparison=rep("j",num.comps),diff=rep(0,num.comps),lwr=rep(0,num.comps),
                    upr=rep(0,num.comps),raw.p=rep(0,num.comps))                # initiate slope comparisons info data.frame
  cdf$comparison <- toString(cdf$comparison)                                    # must convert to strings
  for (i in 1:lev.num) {                                                        # cycle through multiple models
    lm1 <- lm(y~x*g); sum <- summary(lm1)                                       # fit model and get results
    conf <- confint(lm1,level=conf.level)               
    sdf <- extract.slopes(sum,conf,sdf,i,conf.level)                            # get individual slopes information
    if (i < lev.num) {
      cdf <- extract.comparisons(sum,conf,cdf,i,lev.names,lev.num,conf.level)
      g <- relevel(g,lev.names[i+1])                                            # prepare to fit model w/ new reference group
    }
  } # for i
  cdf <- perform.control(control,cdf)
  sdf <- perform.control(control,sdf)
  if (order.slopes) sdf <- sdf[order(sdf$slopes),]
  res <- list(control=control,comparisons=cdf,slopes=sdf,digits=digits)
  class(res) <- "compSlopes"
  res
}

#'@rdname compSlopes
#'@export
print.compSlopes <- function(x,...) {
  cat("\nMultiple Slope Comparisons\n")
  print(x$comparisons,digits=x$digits)
  cat("\nSlope Information\n")
  print(x$slopes,digits=x$digits)
}
