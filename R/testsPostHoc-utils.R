#' @title Places significance letters next to mean points on a fitPlot.
#' 
#' @description Places significance letters next to mean points on an active \code{\link[FSA]{fitPlot}} from a one-way or a two-way ANOVA.
#' 
#' @details The graphic of group means must be active for this function to place the characters next to the group mean points. Typically this graphic is made with the \code{\link[FSA]{fitPlot}} function.
#' 
#' @param mdl A \code{lm} object or formula depicting an ANOVA
#' @param lets A vector of characters to be placed next to each group mean point
#' @param which A character string listing terms in the fitted model for which the means should be calculated and the letters placed
#' @param change.order A logical that is used to change the order of the factors in the \code{lm} object. This is used to change which factor is plotted on the x-axis and which is used to connect means. Used only with a two-way ANOVA and only if the same argument is used with the \code{fitPlot} function
#' @param pos A value or vector of values indicating the positiong to place the characters relative to each group mean point. Note that \code{1}=below, \code{2}=left, \code{3}=above, and \code{4}=right
#' @param offset A value indicating a proportion of character widths to move the character away from each group mean point
#' @param col A single or vector of numeric or character representations of colors used for each character
#' @param cex A single or vector of numeric values used to represent the character expansion values
#' @param \dots Other arguments to be passed to the \code{text} function
#' 
#' @return None. However, an active graphic is modified.
#' 
#' @note Students are often asked to determine which level means are different if a significant one-way or two-way ANOVA is encountered. The results of these multiple comparisons are often illustrated by placing \dQuote{significance letters} next to mean points on an interaction of main effects plot. In R this can be accomplished with a number of \code{text} calls. This function, however, simplifies this process so that newbie students can create the graphic in an efficient and relatively easy manner (in conjunction with \code{\link[FSA]{fitPlot}}). This function thus allows newbie students to interact with and visualize moderately complex linear models in a fairly easy and efficient manner.
#' 
#' @seealso \code{\link[FSA]{fitPlot}}.
#' 
#' @keywords hplot
#' 
#' @examples
#' if (require(FSA)) {
#' data(Mirex)
#' Mirex$year <- factor(Mirex$year)
#' 
#' ## one-way ANOVA
#' lm1 <- lm(mirex~year,data=Mirex)
#' anova(lm1)
#' # suppose multiple comparison results from following
#' \dontrun{
#' mc1 <- multcomp::glht(lm1,mcp(year="Tukey"))
#' summary(mc1)
#' }
#' fitPlot(lm1,main="")
#' addSigLetters(lm1,c("a","a","a","a","ab","b"),pos=c(2,2,4,4,4,4))
#' 
#' # same example, but using cld() from multcomp
#' \dontrun{
#' ( sl1 <- multcomp::cld(mc1) )
#' fitPlot(lm1,main="")
#' addSigLetters(lm1,lets=sl1,pos=c(2,2,4,4,4,4))
#' }
#' 
#' ## two-way ANOVA
#' lm2 <- lm(mirex~year*species,data=Mirex)
#' anova(lm2)
#' 
#' # suppose multiple comparison results from following
#' \dontrun{
#' mc2y <- glht(lm2,mcp(year="Tukey"))
#' summary(mc2y)
#' mc2s <- glht(lm2,mcp(species="Tukey"))
#' summary(mc2s)
#' }
#' op <- par(mfcol=c(1,2))
#' fitPlot(lm2,which="year",type="b",pch=19,ylim=c(0.05,0.35),main="")
#' addSigLetters(lm2,which="year",c("a","a","a","a","ab","b"),pos=c(2,2,4,4,4,4))
#' fitPlot(lm2,which="species",type="b",pch=19,ylim=c(0.05,0.35),main="")
#' addSigLetters(lm2,which="species",c("a","a"),pos=c(2,4))
#' par(op)
#' }
#' 
#' @export
addSigLetters <- function(mdl,lets,which,change.order=FALSE,
                          pos=rep(2,length(mns)),offset=0.5,
                          col=rep(1,length(mns)),cex=rep(0,length(mns)),
                          ...) {
  mdl <- iTypeoflm(mdl)
  if (mdl$type!="ONEWAY" & mdl$type!="TWOWAY")
    stop("\n addSigLetters only works with one or two factors in the model.",
         call.=FALSE)
  if (mdl$type=="ONEWAY" | (mdl$type=="TWOWAY" & !missing(which))) {
    ifelse(missing(which),ord <- 2, ord <- match(which,colnames(mdl$mf)))
    mns <- tapply(mdl$mf[,1],mdl$mf[,ord],mean)
    x <- 1:length(mns)
  } else {
    ifelse(change.order,ord <- c(3,2),ord <- c(2,3))
    mns <- tapply(mdl$mf[,1],mdl$mf[,ord[1]]:mdl$mf[,ord[2]],mean)
    x <- rep(1:length(levels(mdl$mf[,ord[1]])),each=length(levels(mdl$mf[,ord[2]])))
    if (is.ordered(mdl$mf[,ord[1]])) x <- levels(mdl$mf[,ord[1]])[x]
  }
  if (inherits(lets,"cld")) lets <- lets$mcletters$Letters
  if (length(lets) != length(mns)) stop("Length of lets vector is incorrect.",
                                        call.=FALSE)
  if (length(pos) == 1) pos <- rep(pos,length(mns))
    else if (length(pos) != length(mns)) stop("Length of pos vector is incorrect.",
                                              call.=FALSE)
  if (length(col) == 1) col <- rep(col,length(mns))
    else if (length(col) != length(mns)) stop("Length of col vector is incorrect.",
                                              call.=FALSE)
  if (length(cex) == 1) cex <- rep(cex,length(mns))
    else if (length(cex) != length(mns)) stop("Length of cex vector is incorrect.",
                                              call.=FALSE)
  graphics::text(x,mns,lets,pos=pos,offset=offset,col=col,cex=cex,...)  
}




#' @title Tests for significant differences among all pairs of populations in a chi-square test.
#' 
#' @description Tests for significant differences among all pairs of populations in a chi-square test.
#' 
#' @details Post-hoc tests for which pairs of populations differ following a significant chi-square test can be constructed by performing all chi-square tests for all pairs of populations and then adjusting the resulting p-values for inflation due to multiple comparisons. The adjusted p-values can be computed with a wide variety of methods (see \code{\link[stats]{p.adjust}}). This function basically works as a wrapper function that sends the unadjusted \dQuote{raw} p-values from each pair-wise chi-square test to the \code{\link[stats]{p.adjust}} function in the base R program. The \code{\link[stats]{p.adjust}} function should be consulted for further description of the methods used.
#' 
#' @param chi A \code{chisq.test} object
#' @param popsInRows A logical indicating whether the populations form the rows (default; \code{=TRUE}) of the table or not (\code{=FALSE})
#' @param control A string indicating the method of control to use (see details)
#' @param digits A numeric that controls the number of digits to print
#' @param verbose A logical that conrols whether the warning message from the individual \code{chisq.test} calls are printed
#' 
#' @return A data.frame with a description of the pairwise comparisons, the raw p-values, and the adjusted p-values.
#' 
#' @seealso \code{\link[stats]{chisq.test}} and \code{\link[stats]{p.adjust}}.
#' 
#' @keywords htest
#' 
#' @examples
#' # Makes a table of observations -- similar to first example in chisq.test
#' M <- as.table(rbind(c(76, 32, 46), c(48,23,47), c(45,34,78)))
#' dimnames(M) <- list(sex=c("Male","Female","Juv"),loc=c("Lower","Middle","Upper"))
#' M
#' # Fits chi-square test and shows summary
#' ( chi1 <- chisq.test(M) )
#' # Shows post-hoc pairwise comparisons using fdr method
#' chisqPostHoc(chi1)
#' 
#' # Transpose the observed table to demonstrate use of popsInRows=FALSE
#' ( chi2 <- chisq.test(t(M)) )
#' chisqPostHoc(chi2,popsInRows=FALSE)
#' 
#' # How does it handle spares columns
#' ( obs <- matrix(c(20,0,20,30,20,20,10,0,0),nrow=3,byrow=TRUE) )
#' chi1 <- chisq.test(obs)
#' chisqPostHoc(chi1)
#' ( obs <- matrix(c(20,0,0,30,20,20,10,0,0),nrow=3,byrow=TRUE) )
#' chi1 <- chisq.test(obs)
#' chisqPostHoc(chi1)
#' 
#' @export
chisqPostHoc <- function(chi,popsInRows=TRUE,control=stats::p.adjust.methods,digits=4,
                         verbose=TRUE) {
  control <- match.arg(control)
  tbl <- chi$observed
  if (!popsInRows) tbl <- t(tbl)
  popsNames <- rownames(tbl)
  if (is.null(popsNames)) popsNames <- 1:ncol(tbl)
  prs <- utils::combn(1:nrow(tbl),2)
  tests <- ncol(prs)
  pvals <- numeric(tests)
  lbls <- character(tests)
  for (i in 1:tests) {
    ## reduce table to only those pairs
    tmp <- tbl[prs[,i],]
    ## remove columns that are zeroes for both pops
    csums <- colSums(tmp)==0
    if (any(csums)) tmp <- tmp[,!csums]
    if (!is.matrix(tmp)) {
      pvals[i] <- NA
    } else {
      if (!verbose) options(warn=-1)
      pvals[i] <- stats::chisq.test(tmp)$p.value
      if (!verbose) options(warn=0)
    }
    lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
  }
  adj.pvals <- stats::p.adjust(pvals,method=control)
  cat("Adjusted p-values used the",control,"method.\n\n")
  data.frame(comparison=lbls,raw.p=round(pvals,digits),
             adj.p=round(adj.pvals,digits))
}



#' @title Extracts names for significantly different comparisons.
#' 
#' @description Extracts names for significantly different comparisons from a \code{glht} object.
#' 
#' @aliases glhtSig glhtSig.glht
#' 
#' @param object An object saved from \code{\link[multcomp]{glht}} from the \pkg{multcomp} package.
#' @param type A function for computing p-values (see \code{\link[multcomp]{glht}}).
#' @param alpha A numeric indicated the rejection criterion to be used for identifying significant comparisons. Defaults to \code{0.05}.
#' @param \dots Other arguments to pass through to \code{\link[multcomp:methods]{summary.glht}}.
#' 
#' @return A vector containing the names of the paired comparisons that have a p-value less than \code{alpha}.
#' 
#' @note This can be very slow to process. Be patient.
#' 
#' @seealso \code{\link[multcomp]{glht}} and \code{\link[multcomp:methods]{summary.glht}} in \pkg{multcomp}.
#' 
#' @keywords misc
#' 
#' @examples
#' ## NONE YET
#' 
#' @rdname glhtSig
#' @export
glhtSig <- function (object, ...) {
  UseMethod("glhtSig") 
}

#' @rdname glhtSig
#' @export
glhtSig.glht <- function(object,type=c("single-step","Shaffer","Westfall","free",
                                       stats::p.adjust.methods),alpha=0.05,...) {
  test <- multcomp::adjusted(type)
  ts <- test(object)
  names(ts$coefficients)[ts$pvalues<alpha]
}




#' @title Pseudo- confidence intervals for multinomial levels.
#' 
#' @description Pseudo- confidence intervals for the proportions found in the multinomial levels of a goodness-of-fit test computed using \code{\link[stats]{chisq.test}}.
#' 
#' @details Computes confidence intervals for the proportion of the total in each level found in \code{$observed} of the \code{chi} object. The confidence intervals are computed by treating each level as if it is the \dQuote{success} in a binomial confidence interval calculation (using \code{\link[FSA]{binCI}} from \pkg{FSA}). One of three methods for computing each confidence interval can be used and is declared in the \code{type=} argument. The three methods are described in detail for \code{\link[FSA]{binCI}}.
#' 
#' It should be noted that this is NOT the ideal method for computing confidence intervals for multinomial probabilities. This area appears to receive a great deal of discussion, but two methoeds that appear to be generally accepted are due to Sison and Ganz (1995) when the number of cells (k) is \dQuote{large} (i.e., greater than 10) and Goodman (1965) when k is small (see May and Johnson 2000). I have been unable to locate R code for the method of Sison and Ganz (1995) in any existing package, nor could I convert the SAS code provided by May and Johnson (2000) to R code. As of July, 2011 there were at least two queries on R-help for the Sison and Ganz (1995) code with no replies.
#' 
#' This function was developed largely for use by my Introductory Statistics students in order to allow some post hoc discussion regarding significant results in goodness-of-fit tests. This function was not developed for research-grade analyses.
#' 
#' @param chi An object from \code{\link[stats]{chisq.test}} representing a goodness-of-fit test
#' @param conf.level A number indicating the level of confidence to use for constructing confidence intervals (default is \code{0.95})
#' @param type A string that identifies the type of method to use for the calculations (see details)
#' @param digits A number representing the number of digits to which to print the results
#' 
#' @return A kx4 matrix, where k is the number of levels of the categorical variable, containing the observed proportion, the lower and upper confidence interval bounds for the population proportion, and the expected proportion used in the chi-square test.
#' 
#' 
#' @note This function is the Goodman method modified from code provided by Paul Rabie (then at the University  of Minnesota).
#' 
#' @seealso \code{\link[FSA]{binCI}} in \pkg{FSA}.
#' 
#' @references Glaz, J. and C.P. Sison. 1999. Simultaneous confidence intervals for multinomial proportions. Journal of Statistical Planning and Inference 82:251-262.
#' 
#' Goodman, L.A. 1965. On simultaneous confidence intervals for multinomial proportions. Technometrics 7:247-254.
#' 
#' May, W.L. and W.D. Johnson. 2000. Constructing two-sided simultaneous confidence intervals for multinomial proportions for small counts in a large number of cells. Journal of Statistical Software 5(6). Paper and code available at \url{http://www.jstatsoft.org/v05/i06}.
#' 
#' Sison, C.P and J. Glaz. 1995. Simultaneous confidence intervals and sample size determination for multinomial proportions. Journal of the American Statistical Association, 90:366-369. Paper available at \url{http://tx.liberal.ntu.edu.tw/~purplewoo/Literature/!Methodology/!Distribution_SampleSize/SimultConfidIntervJASA.pdf}
#' 
#' Wang, H. 2008. Exact confidence coefficients of simultaneous confidence intervals for multinomial proportions. Journal of Multivariate Analysis 99:896-911.
#' 
#' @keywords htest
#' 
#' @examples
#' # example from chisq.test()
#' x <- c(A = 20, B = 15, C = 25)
#' ( x.chi <- chisq.test(x) )
#' gofCI(x.chi)
#' # note that all CIs contain expected proportions
#' 
#' # Another example from chisq.test()
#' x <- c(89,37,30,28,2)
#' p <- c(40,20,20,15,5)
#' ( x.chi <- chisq.test(x,p=p,rescale.p=TRUE) )
#' gofCI(x.chi)
#' # note that last CIs does not contain the expected proportion
#' 
#' # Same as above but using the Goodman method
#' gofCI(x.chi,type="goodman")
#' 
#' # A chi-square (not goodness-of-fit) test from chisq.test()
#' #M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
#' #dimnames(M) <- list(gender=c("M","F"), party=c("Democrat","Independent", "Republican"))
#' #( Xsq <- chisq.test(M) )
#' #try( gofCI(Xsq) )
#' # Gives an error as gofCI only works for goodness-of-fit results
#' 
#' # An example with only two levels (i.e., binomial situation)
#' obs <- c(A=56,B=34)
#' ( chi1 <- chisq.test(obs) )
#' gofCI(chi1)
#' 
#' # CI results should be the same as binom.test if type="exact" is used
#' gofCI(chi1,type="exact")
#' binom.test(obs)
#' 
#' # CI results should be the same as prop.test if type="wilson" (default)
#' #   and continuity correction is turned off
#' gofCI(chi1)
#' prop.test(obs[1],sum(obs),correct=FALSE)
#' 
#' # CI results should be the same as from normal theory if type="asymptotic"
#' gofCI(chi1,type="asymptotic")
#' p <- obs/sum(obs) 
#' se.p <- sqrt(p[1]*p[2]/sum(obs))
#' rbind(p[1]+c(1,-1)*qnorm(0.025)*se.p,p[2]+c(1,-1)*qnorm(0.025)*se.p)
#' 
#' @export
gofCI <- function(chi,conf.level=0.95,
                  type=c("wilson","exact","asymptotic","goodman"),
                  digits=getOption("digits")) {
  if (!inherits(chi,"htest")) stop("'chi' argument must be the result of chisq.test()",call.=FALSE)
  if (chi$method != "Chi-squared test for given probabilities") {
    stop("'chi' argument must be the result of a goodness-of-fit test using chisq.test()",call.=FALSE) 
  }
  type <- match.arg(type)
  obs <- chi$observed
  lvls <- length(obs)
  if (type!="goodman") {
    # Ogle's ad hoc brute-force methods
    cis <- matrix(NA,nrow=lvls,ncol=2)
    # get CIs
    for (i in 1:lvls) cis[i,] <- suppressWarnings(FSA::binCI(obs[i],sum(obs),conf.level,type))
    res <- cbind(prop.table(obs),cis,chi$expected/sum(chi$expected))
  } else {
    # Goodman's method, modified from code by by Paul Rabie
    # Generally better if lvls<10
    A <- stats::qchisq((1-(1-conf.level)/lvls),1)
    N <- sum(obs)
    lci <- (A+2*obs-(A*(A+4*obs*(N-obs)/N))^0.5)/(2*(N+A))
    uci <- (A+2*obs+(A*(A+4*obs*(N-obs)/N))^0.5)/(2*(N+A))
    res <- cbind(prop.table(obs),lci,uci,chi$expected/sum(chi$expected))
  }
  colnames(res) <- c("p.obs","p.LCI","p.UCI","p.exp")
  rownames(res) <- names(obs)
  round(res,digits)
}




#' @title Plots test statistic and p-value area for z-, t-, and chi-square tests.
#' 
#' @description Plots test statistic and p-value area for z-, t-, and chi-square tests.
#' 
#' @details This produces a plot of the named sampling distribution with the test statistic and p-value areas shown. This plot is used primarily for students  to visualize the calcualtion of the p-value in the \code{\link{z.test}}, \code{\link[stats]{t.test}}, and \code{\link[stats]{chisq.test}} functions. The results from those functions must be saved to an object.
#' 
#' @param x an object saved from \code{\link{z.test}}, \code{\link[stats]{t.test}}, or \code{\link[stats]{chisq.test}}
#' @param smoothness a single-length numeric indicating the number of points for which to construct the plot. The larger the number the smoother the plot will appear
#' @param shade.col a string indicating the color to use for the (primary) shaded area. If the alternative is two.sided then this area is in the same tail as the test statistic
#' @param shade.col2 a string indicating the color to use for the (secondary) shaded area. If the alternative is two.sided then this area is in the opposite tail as the test statistic
#' @param \dots optional arguments that are not implemented in this version
#' 
#' @return None. However, a plot is constructed.
#' 
#' @note Newbie students can benefit greatly from visualizing the position of the test statistic and the \dQuote{area} of the p-value for common statistical tests using the z-, t-, and chi-square tests. This function produces a graphic from the results of \code{z.test}, \code{t.test}, and \code{chisq.test} to show these values. An additional benefit of this function is that newbie students make fewer mistakes in choosing the alternative hypothesis when they can visualize the final result.
#' 
#' @seealso \code{\link{z.test}}, \code{\link[stats]{t.test}}, and \code{\link[stats]{chisq.test}}.
#' 
#' @keywords distribution hplot
#' 
#' @examples
#' ## 1-sample z-test example 
#' z.ex <- z.test(rnorm(25,100,5),99,5)
#' z.ex
#' plot(z.ex)
#' 
#' ## 1-sample t-test example from t.test
#' t1.ex <- t.test(1:10,y=c(7:20))
#' t1.ex
#' plot(t1.ex)
#' 
#' ## 2-sample t-test example from t.test
#' t2.ex <- t.test(extra~group,data=sleep)
#' t2.ex
#' plot(t2.ex)
#' 
#' ## same but with one-tailed alternative (for illustration)
#' t3.ex <- t.test(extra~group,data=sleep,alt="less")
#' t3.ex
#' plot(t3.ex)
#' 
#' ## Chi-square test example from chisq.test
#' chi1.ex <- chisq.test(InsectSprays$count > 7, InsectSprays$spray)
#' chi1.ex
#' plot(chi1.ex)
#' 
#' ## Another Chi-square test example from chisq.test
#' x <- matrix(c(12, 5, 7, 7), ncol = 2)
#' chi2.ex <- chisq.test(x)
#' chi2.ex
#' plot(chi2.ex)
#' 
#' ## Another Chi-square test example from chisq.test
#' x <- c(89,37,30,28,2)
#' p <- c(40,20,20,15,5)
#' chi3.ex <- chisq.test(x, p = p, rescale.p = TRUE)
#' chi3.ex
#' plot(chi3.ex)
#' 
#' @rdname plot.htest
#' @export
plot.htest <- function(x,smoothness=1000,shade.col="red",shade.col2="red3",...) {
  distrib <- xlab <- names(x$statistic)
  if (!distrib %in% c("z","t","X-squared")) stop("Plot.htest only works if test statistic is 'z','t', or 'X-squared'.",call.=FALSE)
  val <- x$statistic
  val1 <- NA
  lower.tail <- FALSE
  if (distrib!="X-squared") {
    if (x$alternative == "less") lower.tail <- TRUE
    else if (x$alternative == "two.sided") {
      ifelse(val < 0, lower.tail <- TRUE, lower.tail <- FALSE)
      val1 <- -1*val
    }
  }
  switch(distrib,
         z= {
           xvals <- sort(c(val,val1,seq(-4,4,length.out=smoothness)))      
           fx <- stats::dnorm(xvals)
           msg <- ""
         },
         t= {
           xvals <- sort(c(val,val1,seq(stats::qt(0.001,df=x$parameter),stats::qt(0.999,df=x$parameter),length.out=smoothness)))
           fx <- stats::dt(xvals,df=x$parameter)
           msg <- paste("df=",round(x$parameter,2),"; ")
         },
         "X-squared"= {
           xvals <- sort(c(val,val1,seq(0,stats::qchisq(0.999,df=x$parameter),length.out=smoothness)))
           fx <- stats::dchisq(xvals,df=x$parameter)
           msg <- paste("df=",x$parameter,";")
         }
  )
  c.region(val,xvals,fx,lower.tail,area=0,plot=TRUE,
           show.ans=FALSE,shade.col=shade.col,lbl.col=shade.col,
           show.lbl=TRUE,main="",xlab=xlab,ylab="fx",yaxt="n")
  if (distrib != "X-squared") {
    if(x$alternative=="two.sided") {
      area2 <- c.region(-1*val,xvals,fx,!lower.tail,area=0,plot=FALSE,
                        shade.col=shade.col,lbl.col=shade.col,show.lbl=TRUE)
      with(area2,graphics::polygon(x.shade,y.shade,col=shade.col2,border=shade.col2))
      with(area2,graphics::lines(xvals,fx))
    }
  }
  graphics::mtext(paste(msg,"p-value=",round(x$p.value,4)),line=0.2)
}




#' @title Shows the predicted value and interval on a fitted line plot.
#' 
#' @description Shows the predicted value and interval on a fitted line plot. This function is used to illustrate predictions with SLR or IVR models and to show distinctions between confidence and prediction intervals.
#' 
#' @details This function produces a fitted line plot with both confidence and prediction bands shown. It then constructs vertical bars representing the predicted values with the corresponding interval (chosen with \code{interval}) for all observations found in \code{newdata}.
#' 
#' This function is only appropriate for SLR and IVR with a single quantitative covariate and two or fewer factors.
#' 
#' The \code{predictPlot()} is just a pass-through to \code{predictionPlot()}.
#' 
#' @aliases predictionPlot predictPlot
#' 
#' @param mdl an \code{lm} or \code{nls} object (i.e., returned from fitting a model with either \code{lm} or \code{nls})
#' @param newdata A data frame in which to look for variables with which to predict. This cannot be omitted as it is with \code{predict}
#' @param interval a string indicating whether to plot confidence (\code{="confidence"}) or prediction (\code{="prediction"}) intervals
#' @param conf.level a decimal numeric indicating the level of confidence to use for confidence and prediction intervals (default is \code{0.95})
#' @param lty a numeric indicating the type of line used for representing the intervals (see \code{par})
#' @param lwd a numeric indicating the width of line used for representing the intervals (see \code{par})
#' @param legend Controls use and placement of the legend (see details in \code{\link[FSA]{fitPlot}})
#' @param \dots Other arguments to the \code{fitPlot} function. For example, include \code{legend=TRUE} to include a legend on the fitted line plot for an IVR.
#' 
#' @return A data.frame is returned that contains the number of the new observation (for comparison to the graphic that is produced), the values of the variables in \code{newdata}, and the predicted values at those observed values.
#' 
#' @seealso \code{predict} specifically \code{predict.lm} and \code{fitPlot} from \pkg{FSA}.
#' 
#' @keywords hplot models
#' 
#' @examples
#' lm1 <- lm(Sepal.Length~Petal.Length*Species,data=iris)
#' lm2 <- lm(Sepal.Length~Petal.Length+Species,data=iris)
#' lm3 <- lm(Sepal.Length~Petal.Length,data=iris)
#' op <- par(mfrow=c(2,2),mar=c(3,3,2,1),mgp=c(2,0.7,0))
#' newdf <- data.frame(Petal.Length=c(2,4),Species=c("setosa","versicolor"))
#' predictionPlot(lm1,newdf,legend="topleft")
#' predictionPlot(lm2,newdf,legend="topleft")
#' predictionPlot(lm3,newdf)
#' predictionPlot(lm3,newdf,interval="confidence")
#' par(op)
#' 
#' @rdname predictionPlot
#' @export predictPlot
predictPlot <- function(...) {
  predictionPlot(...)
}

#' @rdname predictionPlot
#' @export predictionPlot
predictionPlot <- function(mdl,newdata,interval="prediction",conf.level=0.95,
                           lty=1,lwd=3,legend="topright",...) {
  lmtype <- iTypeoflm(mdl)
  if (lmtype$type!="SLR" & lmtype$type!="IVR") stop("\n  Function only works for SLR or IVR models.",call.=FALSE)
  if (dim(lmtype$mf)[2]>4) stop("\n  Function only works for IVR models with one covariate and two or fewer factors.",call.=FALSE)
  if (missing(newdata)) stop("\n newdata argument is missing.",call.=FALSE)
  if (dim(newdata)[1]<1 | dim(newdata)[2]<1) stop("\n newdata is empty",call.=FALSE)
  if (!all(names(lmtype$mf)[-1] %in% names(newdata))) stop("\n Not all variables found in mdl are in newdata.",call.=FALSE)
  
  if (lmtype$type=="SLR") {
    FSA::fitPlot(mdl,interval=interval,cex.main=0.8,conf.level=conf.level,...)
  } else {
    FSA::fitPlot(mdl,interval=interval,cex.main=0.8,conf.level=conf.level,legend=legend,...)
  }
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
