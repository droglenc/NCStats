#' @title Plots calculated areas or values from a wide variety of continuous and discrete distribution functions.
#' 
#' @description Plots calculated areas or values from normal, Student's t, F, chi-square, beta, exponential, and gamma continuous density functions and binomial, geometric, negative binomial, Poisson, and hypergeometric discrete distribution functions.
#' 
#' @details This function computes the same values as the \code{pXXX} and \code{qXXX} functions for both continuous density and discrete probability distributions and as the \code{dXXX} functions for the discrete probability distributions.  This function is used primarily to include a plot with the calculation.  The original functions in the \pkg{stats} package should be consulted for more details on each distribution.  For example, use \code{?dbinom} to get help for computing the probability of a value in the binomial distribution, \code{?ppois} to get help for computing the cumulative probability of a value on the Poisson distribution, and \code{?qnorm} to get help for computing the quantile value of a normal distribution.
#' 
#' The strings in the \code{distrib} argument are the same as the abbreviated function names in the \pkg{stats} package.  For example, the normal distribution is \code{"norm"}, the chi-square distribution is \code{"chisq"}, and the hypergeometric distribution is \code{"hyper"}
#' 
#' The strings in the \code{type} argument at the same as the prefixes to the abbreviated function names in the \pkg{stats} package.  For example, the density is computed with \code{"d"}, the cumulative probability with \code{"p"}, and the quantiles with \code{"q"}.  The \code{"forward"} and \code{"reverse"} types correspond to \code{"p"} and \code{"q"}, respectively, and are used to match terminology used in the Introductory Statistics course at Northland College.
#' 
#' Other visualizations of the distribution can be seen with functions that are named with the \code{s} prefix (for simulation) on the abbreviated function name.  For example, try \code{sbinom()} to show a simulation of the binomial distribution.
#' 
#' Note that for a normal distribution the \code{se=} argument is supplied for convenience only.  Whatever is supplied to \code{se=} is then applied to the \code{sd=} argument.  Use of \code{se=} may be somewhat simpler for students learning to distinguish between normal distribution calculations on a sampling versus a population distribution.
#' 
#' @param val a single-length numeric containing a value or probability/quantile.
#' @param distrib a string indicating the type of distribution to use.  See details.
#' @param type a string indicating the type of calculation to make -- \code{"p"} or \code{"forward"} is the cumulative probability given a value, \code{"q"} or \code{"reverse"} is the value given a probability/quantile, and \code{"d"} is the probability for a given value on a discrete distribution function (see details)
#' @param digits A numeric indicating the number of decimals to round the returned result when \code{plot=FALSE}.
#' @param mean a single-length numeric for the mean of the normal distribution.
#' @param sd a single-length numeric for the standard deviation of the normal distribution.
#' @param se a single-length numeric for the standard error of the normal distribution (see details)
#' @param df a single-length numeric for the degrees-of-freedom for the t and chi-square distributions.
#' @param df1 a single-length numeric for the numerator degrees-of-freedom for the F distribution.
#' @param df2 a single-length numeric for the denominator degrees-of-freedom for the F distribution.
#' @param shape1 a single-length numeric for the first shape parameter in the beta distribution.
#' @param shape2 a single-length numeric for the first shape parameter in the beta distribution.
#' @param shape a single-length numeric for the shape parameter in the gamma distribution.
#' @param scale a single-length numeric for the scale parameter in the gamma distribution.
#' @param rate a single-length numeric for the rate parameter in the gamma and exponential distributions.  See details.
#' @param size a single-length numeric for the number of trials in the binomial distribution.
#' @param prob a single-length numeric for the probability of success on each trial in the binomial, geometric, and negative binomial distributions.
#' @param mu a single-length numeric for the mean of negative binomial distribution. see details
#' @param lambda a single-length numeric for the mean of the Poisson distribution.
#' @param m a single-length numeric for the number of white balls in the urn of a hypergeometric distribution.
#' @param n a single-length numeric for the number of black balls in the urn of a hypergeometric distribution.
#' @param k a single-length numeric for the number of balls drawn from the urn of a hypergeometric distribution.
#' @param lower.tail a logical indicating if the probabilities are P[X<=x] (\code{=TRUE}; default) or P[X>x].
#' @param plot a logical indicating if a plot should be shown (\code{TRUE}) or not.
#' @param show.ans a logical indicating if the computational results should be shown on the plot (\code{TRUE}) or not.
#' @param show.alt a logical indicating if a message showing alternaive (i.e,. traditional) code for obtaining the answer should be shown (\code{TRUE}) or not (default).
#' @param main a character string for the plot's main label.  If null an appropriate string will be created.
#' @param xlab character string for plot's x-axis label.  If null an appropriate string will be created.
#' @param ylab character string for plot's y-axis label.  If null an appropriate string will be created.
#' @param yaxt a character which specifies the y axis type.  Specifying \code{"n"} (default) suppresses plotting.  Specifying \code{NULL} gives axis labels.
#' @param smoothness a single-length numeric indicating the number of points for which to construct the plot.  The larger the number the smoother the plot will appear.
#' @param shade.col a string indicating the color to use for the shaded area.
#' @param shade.trans Value between 0 and 1 indicating the transparency to use for the shaded area.
#' @param lbl.col a string indicating the color to use for labeling the shaded area.
#' @param \dots optional parameters to send to the internal \code{c.region} or \code{d.region} functions.  For example, can change the size of the answer with \code{cex.ans=}
#' 
#' @return A numeric answer is returned.  If \code{plot=TRUE} then a plot is constructed and a note on how to use one of the functions in \pkg{stats} is printed if \code{show.alt=TRUE}.
#' 
#' @note Students often need to \dQuote{look up} values or probabilities from statistical tables in textbooks.  These values can be easily found in R with the \code{pXXX}, \code{qXXX}, or \code{dXXX} distributional functions.  However, these functions do not provide a visual representation of the computation.  This function also replaces the tables of distributional values found in texts but also provides a graph of the computation.
#' 
#' I had hoped to add a \code{plot=} argument to any of the \code{pXXX}, \code{qXXX}, or \code{dXXX} distributional functions but this required replacing the base R functions.  A compromise was to create this new function, using the \code{type=} argument with \code{p}, \code{q}, and \code{d} and printing a direction on how to use the base R function if \code{show.alt=TRUE}.
#' 
#' @seealso The \code{type=} and \code{distrib=} arguments can be concatenated to provide the typical function in base R.  For example, for the examples below see \code{\link{dbinom}}, \code{\link{dpois}}, \code{\link{pexp}}, and \code{\link{qnorm}}.
#' 
#' @keywords distribution hplot
#' 
#' @examples
#' op <- par(mfrow=c(2,2),mar=c(3.5,1,3.5,1),mgp=c(2,0.75,0))
#' distrib(4,distrib="binom",type="d",size=10,prob=0.5)
#' distrib(5,distrib="pois",type="p",lambda=2)
#' distrib(2.2,distrib="exp",type="p",rate=1/2)
#' distrib(0.6,distrib="norm",type="q",mean=10,sd=3,lower.tail=FALSE)
#' par(op)
#' 
#' @export
distrib <- function(val,distrib=c("norm","t","chisq","f","beta","exp","gamma","pois",
                                  "binom","geom","hypergeom","nbinom"),
                   type=c("p","q","d","forward","reverse"),digits=getOption("digits"),
                   mean=0,sd=se,se=1,df=1,df1=df,df2=1,shape1=1,shape2=1,shape=1,
                   rate=1,scale=1/rate,size=10,prob=0.5,mu=NULL,lambda=1,
                   m=NULL,n=NULL,k=NULL,lower.tail=TRUE,plot=TRUE,show.ans=TRUE,
                   show.alt=FALSE,main=NULL,xlab=NULL,ylab=NULL,yaxt="n",
                   smoothness=1000,shade.col="red",shade.trans=1,lbl.col="red",...) {
  distrib <- match.arg(distrib)
  type <- match.arg(type)
  if (!is.logical(lower.tail)) stop("'lower.tail' must be a logical (i.e., TRUE or FALSE without quotes).",call.=FALSE)
  if (shade.trans!=1) shade.col <- FSA::col2rgbt(shade.col,shade.trans)
  if (type=="forward") type <- "p"
    else if (type=="reverse") type <- "q"
  if (type=="q" & (val<=0 | val>=1)) stop("When solving for a value of X (type='q') then area (val=) must be between 0 and 1.",call.=FALSE)
  if (length(mean)>1 | length(sd)>1 | length(df)>1 | length(df1)>1 | length(df2)>1 | length(shape1)>1 | length(shape2)>1 | length(shape)>1 | length(rate)>1 | length(scale)>1 | length(size)>1 | length(prob)>1 | length(mu)>1 | length(lambda)>1 | length(m)>1 | length(n)>1 | length(k)>1) stop("All parameter arguments must have length not greater than 1.",call.=FALSE)
  ifelse(distrib %in% c("norm","t","chisq","f","beta","exp","gamma"),continuous <- TRUE,continuous <- FALSE) 
  if (type=="d" & continuous & plot) stop(paste0("No plot will be constructed for the density (type='d') of a continuous distribution.  Use ",type,distrib,"() instead."),call.=FALSE)
  if (length(val)>1) {
    warning(paste0("This function only works for one value.  Use ",type,distrib,"() for multiple values.\nWill continue with your first value."),call.=FALSE)
    val <- val[1]
  }
  switch(distrib,
    norm= {
      if (is.null(main)) main <- bquote(paste("N(",mu==.(mean),",",sigma==.(round(sd,3)),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      if (type=="p") {
        ans <- stats::pnorm(val,mean=mean,sd=sd,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qnorm(val,mean=mean,sd=sd,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(mean-4*sd,mean+4*sd,length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x     
        fx <- stats::dnorm(x,mean=mean,sd=sd)
      }
      msg <- paste0(type,distrib,"(",val,",mean=",mean,",sd=",sd,",lower.tail=",lower.tail,")")
    },
    t={
      if (is.null(main)) main <- bquote(paste(t[.(df)]," Distribution"))
      if (is.null(xlab)) xlab <- "t"
      if (type=="p") {
        ans <- stats::pt(val,df=df,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qt(val,df=df,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(stats::qt(0.001,df=df),stats::qt(0.999,df=df),length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x     
        fx <- stats::dt(x,df=df)
      }
      msg <- paste0(type,distrib,"(",val,",df=",df,",lower.tail=",lower.tail,")")
    },
    chisq={
      if (is.null(main)) main <- bquote(paste(chi[.(df)]^2," Distribution"))
      if (is.null(xlab)) xlab <- expression(chi^2)
      if (type=="p") {
        ans <- stats::pchisq(val,df=df,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qchisq(val,df=df,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(0.01,stats::qchisq(0.999,df=df),length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x
        fx <- stats::dchisq(x,df=df)
      }
      msg <- paste0(type,distrib,"(",val,",df=",df,",lower.tail=",lower.tail,")")
    },
    f={
      if (is.null(main)) main <- "F Distribution"
      if (is.null(xlab)) xlab <- "F"
      if (type=="p") {
        ans <- stats::pf(val,df1=df1,df2=df2,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qf(val,df1=df1,df2=df2,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(0.001,stats::qf(0.994,df1=df1,df2=df2),length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x
        fx <- stats::df(x,df1=df1,df2=df2)
      }
      msg <- paste0(type,distrib,"(",val,",df1=",df1,",df2=",df2,",lower.tail=",lower.tail,")")
    },
    beta={
      if (is.null(main)) main <- bquote(paste("Beta(shape1=",.(shape1),",shape2=",.(shape2),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      if (type=="p") {
        ans <- stats::pbeta(val,shape1=shape1,shape2=shape2,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qbeta(val,shape1=shape1,shape2=shape2,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(0,1,length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x
        fx <- stats::dbeta(x,shape1=shape1,shape2=shape2)
      }
      msg <- paste0(type,distrib,"(",val,",shape1=",shape1,",shape2=",shape2,",lower.tail=",lower.tail,")")
    },
    exp={
      if (is.null(main)) main <- bquote(paste("Exponential(rate=",.(rate),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      if (type=="p") {
        ans <- stats::pexp(val,rate=rate,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qexp(val,rate=rate,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(0,stats::qexp(0.999,rate),length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x
        fx <- stats::dexp(x,rate=rate)
      }
      msg <- paste0(type,distrib,"(",val,",rate=",rate,",lower.tail=",lower.tail,")")
    },
    gamma={
      if (is.null(main)) main <- bquote(paste("Gamma(shape=",.(shape),",scale=",.(scale),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      if (type=="p") {
        ans <- stats::pgamma(val,shape=shape,scale=scale,lower.tail=lower.tail)
        xval <- val
        area <- ans
      } else {
        ans <- stats::qgamma(val,shape=shape,scale=scale,lower.tail=lower.tail)
        xval <- ans
        area <- val
      }
      if (plot) {
        x <- seq(0,1,length.out=smoothness)
        if (!xval %in% x) x <- sort(c(xval,x))   # needed in case xval == one of x
        fx <- stats::dgamma(x,shape=shape,scale=scale)
      }
      msg <- paste0(type,distrib,"(",val,",shape=",shape,",scale=",scale,",lower.tail=",lower.tail,")")
    },
    binom={
      if (is.null(main)) main <- bquote(paste("Binomial(n=",.(size),",p=",.(prob),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      reverse <- FALSE
      if (type=="p") {
        ans <- stats::pbinom(val,size=size,prob=prob,lower.tail=lower.tail)
        xval <- val
        area <- ans
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
      } else if (type=="q") {
        ans <- stats::qbinom(val,size=size,prob=prob,lower.tail=lower.tail)
        xval <- ans
        area <- val
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
        reverse <- TRUE
      } else {
        ans <- stats::dbinom(val,size=size,prob=prob)
        xval <- val
        area <- ans
        reg <- "single"
      }
      if (plot) {
        x <- 0:size
        fx <- stats::dbinom(x,size=size,prob=prob)
      }
      if (type=="d") msg <- paste0(type,distrib,"(",val,",size=",size,",prob=",prob,")")
        else msg <- paste0(type,distrib,"(",val,",size=",size,",prob=",prob,",lower.tail=",lower.tail,")")
    },
    geom={
      if (is.null(main)) main <- bquote(paste("Geometric(p=",.(prob),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      reverse <- FALSE
      if (type=="p") {
        ans <- stats::pgeom(val,prob=prob,lower.tail=lower.tail)
        xval <- val
        area <- ans
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
      } else if (type=="q") {
        ans <- stats::qgeom(val,prob=prob,lower.tail=lower.tail)
        xval <- ans
        area <- val
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
        reverse <- TRUE
      } else {
        ans <- stats::dgeom(val,prob=prob)
        xval <- val
        area <- ans
        reg <- "single"
      }
      if (plot) {
        x <- 0:stats::qgeom(0.9999,prob=prob)
        fx <- stats::dgeom(x,prob=prob)
      }
      if (type=="d") msg <- paste0(type,distrib,"(",val,",prob=",prob,")")
        else msg <- paste0(type,distrib,"(",val,",prob=",prob,",lower.tail=",lower.tail,")")
    },
    hypergeom={
      if (is.null(n) | is.null(m) | is.null(k)) stop("All of n, m, and k arguments must be non-null for a hypergeometric distribution.",call.=FALSE)
      if (k > (n+m)) stop("The number of items drawn (k) cannot be larger then the number of items in the urn (m+n).",call.=FALSE)
      if (is.null(main)) main <- bquote(paste("Hypergeometric(m=",.(m),"n=",.(n),"k=",.(k),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      reverse <- FALSE
      if (type=="p") {
        ans <- stats::phyper(val,m=m,n=n,k=k,lower.tail=lower.tail)
        xval <- val
        area <- ans
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
      } else if (type=="q") {
        ans <- stats::qhyper(val,m=m,n=n,k=k,lower.tail=lower.tail)
        xval <- ans
        area <- val
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
        reverse <- TRUE
      } else {
        ans <- stats::dhyper(val,m=m,n=n,k=k)
        xval <- val
        area <- ans
        reg <- "single"
      }
      if (plot) {
        x <- 0:k
        fx <- stats::dhyper(x,m=m,n=n,k=k)
      }
      if (type=="d") msg <- paste0(type,distrib,"(",val,",m=",m,",n=",n,"k=",k,")")
        else msg <- paste0(type,distrib,"(",val,",m=",m,",n=",n,"k=",k,",lower.tail=",lower.tail,")")
    },
    nbinom={
      if (!is.null(mu)) {
        prob <- size/(size + mu)      
        if (is.null(main)) main <- bquote(paste("Negative Binomial(n=",.(size),mu==.(mu),") Distribution"))
      } else if (is.null(main)) main <- bquote(paste("Negative Binomial(n=",.(size),"p=",.(prob),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      reverse <- FALSE
      if (type=="p") {
        ans <- stats::pnbinom(val,size=size,prob=prob,lower.tail=lower.tail)
        xval <- val
        area <- ans
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
      } else if (type=="q") {
        ans <- stats::qnbinom(val,size=size,prob=prob,lower.tail=lower.tail)
        xval <- ans
        area <- val
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
        reverse <- TRUE
      } else {
        ans <- stats::dnbinom(val,size=size,prob=prob)
        xval <- val
        area <- ans
        reg <- "single"
      }
      if (plot) {
        x <- 0:stats::qnbinom(0.9999,size=size,prob=prob)
        fx <- stats::dnbinom(x,size=size,prob=prob)
      }
      if (is.null(mu)) {
        if (type=="d") msg <- paste0(type,distrib,"(",val,",size=",size,",prob=",prob,")")
          else msg <- paste0(type,distrib,"(",val,",size=",size,",prob=",prob,",lower.tail=",lower.tail,")")
      } else {
        if (type=="d") msg <- paste0(type,distrib,"(",val,",size=",size,",mu=",mu,")")
          else msg <- paste0(type,distrib,"(",val,",size=",size,",mu=",mu,",lower.tail=",lower.tail,")")
      }
    },
    pois={
      if (is.null(main)) main <- bquote(paste("Poisson(",lambda==.(lambda),") Distribution"))
      if (is.null(xlab)) xlab <- "X"
      reverse <- FALSE
      if (type=="p") {
        ans <- stats::ppois(val,lambda=lambda,lower.tail=lower.tail)
        xval <- val
        area <- ans
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
      } else if (type=="q") {
        ans <- stats::qpois(val,lambda=lambda,lower.tail=lower.tail)
        xval <- ans
        area <- val
        ifelse(lower.tail,reg <- "lower",reg <- "upper")
        reverse <- TRUE
      } else {
        ans <- stats::dpois(val,lambda=lambda)
        xval <- val
        area <- ans
        reg <- "single"
      }
      if (plot) {
        x <- 0:stats::qpois(0.9999,lambda=lambda)
        fx <- stats::dpois(x,lambda=lambda)
      }
      if (type=="d") msg <- paste0(type,distrib,"(",val,",lambda=",lambda,")")
        else msg <- paste0(type,distrib,"(",val,",lambda=",lambda,",lower.tail=",lower.tail,")")
    }
  ) # end switch(distrib)

  if (show.alt) cat("Could use:",msg,"\n")
  if (plot) {
    withr::local_par(list(yaxs="i",xaxs="i"))
    if (continuous) {
      c.region(xval,x,fx,lower.tail,area,plot=TRUE,show.ans=show.ans,
               shade.col=shade.col,lbl.col=lbl.col,show.lbl=TRUE,
               main=main,xlab=xlab,ylab=ylab,yaxt=yaxt,xaxt="n",
               ylim=c(0,1.02*max(fx)),...)
      if (distrib=="norm") {
        vals <- mean+seq(-4,4,1)*sd
        graphics::axis(1,at=vals,labels=formatC(vals,format="g",digits=3))
      } else graphics::axis(1)
      graphics::box()
    } else {
      d.region(xval,x,fx,reg,area,reverse=reverse,plot=TRUE,show.ans=show.ans,
               shade.col=shade.col,lbl.col=lbl.col,
               main=main,xlab=xlab,ylab=ylab,yaxt=yaxt,
               ylim=c(0,1.02*max(fx)),...)
    }
    invisible(ans)
  } else round(ans,digits)
}