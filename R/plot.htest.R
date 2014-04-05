#'Plots test statistic and p-value area for z-, t-, and chi-square tests.
#'
#'Plots test statistic and p-value area for z-, t-, and chi-square tests.
#'
#'This produces a plot of the named sampling distribution with the test
#'statistic and p-value areas shown.  This plot is used primarily for students
#'to visualize the calcualtion of the p-value in the \code{z.test},
#'\code{t.test}, and \code{chisq.test} functions.  The results from those
#'functions must be saved to an object.
#'
#'@param x an object saved from \code{z.test}, \code{t.test} or
#'\code{chisq.test}
#'@param smoothness a single-length numeric indicating the number of points for
#'which to construct the plot.  The larger the number the smoother the plot
#'will appear.
#'@param shade.col a string indicating the color to use for the (primary)
#'shaded area.  If the alternative is two.sided then this area is in the same
#'tail as the test statistic.
#'@param shade.col2 a string indicating the color to use for the (secondary)
#'shaded area.  If the alternative is two.sided then this area is in the
#'opposite tail as the test statistic.
#'@param \dots optional arguments that are not implemented in this version.
#'@return None.  However, a plot is constructed.
#'@note Newbie students can benefit greatly from visualizing the position of
#'the test statistic and the \dQuote{area} of the p-value for common
#'statistical tests using the z-, t-, and chi-square tests.  This function
#'produces a graphic from the results of \code{z.test}, \code{t.test}, and
#'\code{chisq.test} to show these values.  An additional benefit of this
#'function is that newbie students make fewer mistakes in choosing the
#'alternative hypothesis when they can visualize the final result.
#'@seealso \code{z.test} in \pkg{TeachingDemos} and \code{\link{t.test}} and
#'\code{\link{chisq.test}}.
#'@keywords distribution hplot
#'@examples
#'## 1-sample z-test example 
#'z.ex <- z.test(rnorm(25,100,5),99,5)
#'z.ex
#'plot(z.ex)
#'
#'## 1-sample t-test example from t.test
#'t1.ex <- t.test(1:10,y=c(7:20))
#'t1.ex
#'plot(t1.ex)
#'
#'## 2-sample t-test example from t.test
#'t2.ex <- t.test(extra~group,data=sleep)
#'t2.ex
#'plot(t2.ex)
#'
#'## same but with one-tailed alternative (for illustration)
#'t3.ex <- t.test(extra~group,data=sleep,alt="less")
#'t3.ex
#'plot(t3.ex)
#'
#'## Chi-square test example from chisq.test
#'chi1.ex <- chisq.test(InsectSprays$count > 7, InsectSprays$spray)
#'chi1.ex
#'plot(chi1.ex)
#'
#'## Another Chi-square test example from chisq.test
#'x <- matrix(c(12, 5, 7, 7), ncol = 2)
#'chi2.ex <- chisq.test(x)
#'chi2.ex
#'plot(chi2.ex)
#'
#'## Another Chi-square test example from chisq.test
#'x <- c(89,37,30,28,2)
#'p <- c(40,20,20,15,5)
#'chi3.ex <- chisq.test(x, p = p, rescale.p = TRUE)
#'chi3.ex
#'plot(chi3.ex)
#'
#'@rdname plot.htest
#'@method plot htest
#'@S3method plot htest
plot.htest <- function(x,smoothness=1000,shade.col="red",shade.col2="red3",...) {
  distrib <- xlab <- names(x$statistic)
  if (distrib %nin% c("z","t","X-squared")) stop("Plot.htest only works if test statistic is 'z','t', or 'X-squared'.",call.=FALSE)
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
      fx <- dnorm(xvals)
      msg <- ""
    },
    t= {
      xvals <- sort(c(val,val1,seq(qt(0.001,df=x$parameter),qt(0.999,df=x$parameter),length.out=smoothness)))
      fx <- dt(xvals,df=x$parameter)
      msg <- paste("df=",round(x$parameter,2),"; ")
    },
    "X-squared"= {
      xvals <- sort(c(val,val1,seq(0,qchisq(0.999,df=x$parameter),length.out=smoothness)))
      fx <- dchisq(xvals,df=x$parameter)
      msg <- paste("df=",x$parameter,";")
    }
  )
  c.region(val,xvals,fx,lower.tail,area=0,plot=TRUE,show.ans=FALSE,shade.col=shade.col,lbl.col=shade.col,show.lbl=TRUE,main="",xlab=xlab,ylab="fx",yaxt="n")
  if (distrib != "X-squared") {
    if(x$alternative=="two.sided") {
      area2 <- c.region(-1*val,xvals,fx,!lower.tail,area=0,plot=FALSE,shade.col=shade.col,lbl.col=shade.col,show.lbl=TRUE)
      with(area2,polygon(x.shade,y.shade,col=shade.col2,border=shade.col2))
      with(area2,lines(xvals,fx))
    }
  }
  mtext(paste(msg,"p-value=",round(x$p.value,4)),line=0.2)
}
