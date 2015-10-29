#' @title A dynamic graph to illustrate the concept of confidence intervals for a mean.
#' 
#' @description This function demonstrates the concept of confidence regions by drawing a large number of random samples from a known normal distribution, computing a confidence interval for each sample, and plotting those confidence intervals.  Sliders then let the user change the level of confidence, the sample size, or the type of confidence region (interval or bound) to see how that effects the confidence interval widths (margin-of-error) and capture probability.
#' 
#' @details If the user is using RStudio and the \pkg{manipulate} package is installed then the dynamic graph is produced in the \dQuote{Plots} pane of RStudio.  The plot controls may be accessed through the \dQuote{gear} that is in the upper-left corner of the plot.  If the user is not using RStudio or the \pkg{manipulate} package is not installed, then an attempt is made to produce the dynamic graph with Tcl/Tk using the functions in the \pkg{relax} package.
#' 
#' @param reps A single numeric that indicates the number of replicate samples to draw from the population
#' @param method A single string that indicates whether to make confidence intervals using a normal (\code{="Z"}) or t (\code{="t"}) distribution
#' @param mu A single numeric that indicates the mean of the known normal distribution
#' @param sigma A single numeric that indicates the standard deviation of the known normal distribution
#' 
#' @return None, but a dynamic graphic with sliders is produced.
#' 
#' @keywords misc dynamic
#' 
#' @examples
#' \dontrun{
#' # Default using normal theory for confidence regions
#' ciSim()
#' # Using t-distribution theory for confidence regions
#' ciSim(method="t")
#' }
#' 
#' @export
ciSim <- function(reps=200,method=c("Z","t"),mu=100,sigma=10) {
  ## Trying to fix "no visible bindings" problem for Check
  n <- conf.level <- alternative <- NULL
  method <- match.arg(method)
  ## use manipulate if RStudio is being used.
  if (iCheckRStudio() & requireNamespace("manipulate",quietly=TRUE)) {
    rerand <- TRUE
    manipulate::manipulate(
      {
      if (rerand) set.seed(sample(1:10000))
      iCISimPlot(n,conf.level,alternative,reps,method,mu,sigma)
      },
        n=manipulate::slider(10,100,step=5,initial=10),
        conf.level=manipulate::picker(0.80,0.90,0.95,0.99),
        alternative=manipulate::picker("two.sided","less","greater"),
        rerand=manipulate::button("Rerandomize")
    ) # end manipulate
  } else {
    ## Internal plot refresher function
    iCIRefresh <- function(...) {
      n <- relax::slider(no=1)
      conf <- relax::slider(no=2)
      alternative <- relax::slider(no=3)
      # change alternative number to character
      alternative <- ifelse(alternative==-1,"less",ifelse(alternative==0,"two.sided","greater"))
      iCISimPlot(n,conf,alternative,reps,method,mu,sigma)
    } ## end iCIRefresh internal function
    if (iChk4Namespace("relax")) {
      relax::gslider(iCIRefresh,prompt=TRUE,
                     sl.names=   c( "n", "Confidence (C)", "alternative Type (-1=less,1=grtr)"),
                     sl.mins=    c(  10,             0.80,                -1),
                     sl.maxs=    c( 100,             0.99,                 1),
                     sl.deltas=  c(   5,             0.01,                 1),
                     sl.defaults=c(  10,             0.95,                 0),
                     title = "Confidence Region Simulator",
                     but.functions= function(...){
                       relax::slider(obj.name="rerand",obj.value="Y")
                       iCIRefresh()
                     },
                     but.names=c("Rerandomize"),
                     pos.of.panel="left",vscale=1.5)
    }
  }
}


## Internal function for actually making the plot
iCISimPlot <- function(n,conf,alternative=c("two.sided","less","greater"),
                       reps,method,mu,sigma,...) {
  alternative <- match.arg(alternative)
  ## Create samples, summaries, and CI
  # Sets x range of plot, allows to see change in width of CIs
  xr <- mu+c(-5,5)*sigma/sqrt(10)
  #  random samples from popn
  rnd.reps <- matrix(stats::rnorm(n*reps,mu,sigma),nrow=n)
  #  compute means of random samples
  rnd.mns <- apply(rnd.reps,2,mean)
  # Make CIs for each resample
  if (method=="Z") {
    SE = rep(sigma/sqrt(n),reps)
    switch(alternative,
           less = {
             crit <- stats::qnorm(conf); zstar <- mu-crit*sigma/sqrt(n)
             uci <- rnd.mns+crit*SE; lci <- rep(xr[1],reps)
           },
           greater = {
             crit <- stats::qnorm(conf); zstar <- mu+crit*sigma/sqrt(n)
             lci <- rnd.mns-crit*SE; uci <- rep(xr[2],reps)
           }, 
           two.sided = {
             crit <- stats::qnorm(0.5+conf/2); zstar <- mu+c(-1,1)*crit*sigma/sqrt(n)
             lci <- rnd.mns-crit*SE; uci <- rnd.mns+crit*SE
           } )
    me <- crit*sigma/sqrt(n)
  } else {
    SE = apply(rnd.reps,2,function(x) stats::sd(x)/sqrt(length(x)))
    switch(alternative,
           less = {
             crit <- stats::qt(conf,df=n-1)
             uci <- rnd.mns+crit*SE; lci <- rep(min(xr),reps)
           },
           greater = {
             crit <- stats::qt(conf,df=n-1)
             lci <- rnd.mns-crit*SE; uci <- rep(max(xr),reps)
           }, 
           two.sided = {
             crit <- stats::qt(0.5+conf/2,df=n-1)
             lci <- rnd.mns-crit*SE; uci <- rnd.mns+crit*SE
           }  )
    me <- mean(crit*SE)
  }
  # Identify if a CI contains mu (black) or not (red)
  colr <- ifelse(((lci>mu) | (uci<mu)),"red","black")
  # Percent CIs contained mu
  hit <- paste0(formatC(100*length(colr[colr=="black"])/reps,format="f",digits=0))
  me <- formatC(me,format="f",digits=3)
  
  ## Construct the graphic
  # Some preparations
  old.par <- graphics::par(mar=c(3,1,2,1),mgp=c(2,0.4,0),tcl=-0.2)
  xlbl <- expression(paste("Sample Mean ( ",bar(X)," )"))
  tmp <- ifelse(method=="Z","","avg ")
  title <- substitute(paste(hit,"% contain ",mu," (=",muval,"), ",tmp,"m.e.=",me,),
                      list(hit=hit,tmp=tmp,me=me,muval=mu))
  # make skeleton plot
  graphics::plot(lci,seq(1,reps),type="n",yaxt="n",xlim=xr,
                 xlab=xlbl,ylab="",main=title)
  # Put blue vertical line and label at mu
  graphics::lines(c(mu,mu),c(-0.1,1.2*reps),col="blue",lwd=3,lty=2)
  graphics::text(mu,-1.5,expression(mu),cex=1.25,col="blue")
  # For z method only, put transparent green vertical lines at
  #   crit values of popn.
  if (method=="Z") graphics::abline(v=zstar,col="green",lwd=3,lty=2)
  for (i in 1:reps) {
    # Put CIs on the plot
    graphics::lines(c(lci[i],uci[i]),rep(i,2),col=colr[i])
    # Put means on the plot
    graphics::points(rnd.mns[i],i,pch=19,col=FSA::col2rgbt(colr[i],0.7),cex=0.75)
  }
  graphics::par(old.par)
} # end iCISimPlot internal function



#' @title A dynamic graph to illustrate the Central Limit Theorem.
#' 
#' @description A dynamic graph to illustrate the central limit theorem (CLT).  The user can change the population distribution that is sampled from and the sample size.
#' 
#' @details This function produces two graphics.  The left graphic is a histogram of the individuals in the population and the right graphic is a histogram of the simulated sampling distribution (i.e., means from the multiple samples).  The right graphic may include a normal distribution density curve with \code{incl.norm=TRUE}.
#' 
#' The two graphs are dynamically controlled by three sliders.  The first two sliders control the shape parameters of the beta distribution used to model the population distribution sampled from.  A wide variety of shapes may be created with these two shape parameters.  The third slider controls the size of each sample taken from the population distribution.  The sliders may be used to detect how changes in the shape of the population and the size of the sample effect the shape, center, and dispersion of the sampling distribution.
#' 
#' @details If the user is using RStudio and the \pkg{manipulate} package is installed then the dynamic graph is produced in the \dQuote{Plots} pane of RStudio.  The plot controls may be accessed through the \dQuote{gear} that is in the upper-left corner of the plot.  If the user is not using RStudio or the \pkg{manipulate} package is not installed, then an attempt is made to produce the dynamic graph with Tcl/Tk using the functions in the \pkg{relax} package.
#' 
#' @param reps Number of resamples to take from the population distribution and means to calculate
#' @param incl.norm A logical that indicates whether a normal density curve should be superimposed on the sampling distribution
#' 
#' @return None, but a dynamic graph with sliders is produced.
#' 
#' @keywords misc dynamic
#' 
#' @examples
#' \dontrun{
#' cltSim()
#' }
#' 
#' @export
cltSim <- function(reps=1000,incl.norm=FALSE) {
  ## Trying to fix "no visible bindings" problem for Check
  n <- shape1 <- shape2 <- NULL
  if (iCheckRStudio() & requireNamespace("manipulate",quietly=TRUE)) {
    rerand <- TRUE
    manipulate::manipulate(
      {
        if (rerand) set.seed(sample(1:10000))
        iCLTSimPlot(n,shape1,shape2,reps,incl.norm)
      },
      n=manipulate::slider(10,100,step=5,initial=10),
      shape1=manipulate::slider(1,20,step=1,label="Shape 1 (alpha)"),
      shape2=manipulate::slider(1,20,step=1,label="Shape 2 (beta)"),
      rerand=manipulate::button("Rerandomize")
    ) # end manipulate
  } else {
    ## internal referesher function
    iCLTrefresh <- function(...) {
      n <- relax::slider(no=1)
      shape1 <- relax::slider(no=2)
      shape2 <- relax::slider(no=3)
      iCLTSimPlot(n,shape1,shape2,reps,incl.norm)
    } # end iCLTrefresh internal function
    if (iChk4Namespace("relax")) {
      relax::gslider(iCLTrefresh,prompt=TRUE,hscale=2,
                     sl.names=   c( "n", "Shape 1 (alpha)", "Shape 2 (beta)"),
                     sl.mins=    c(  10,                 1,                1),
                     sl.maxs=    c( 100,                20,               20),
                     sl.deltas=  c(   5,                 1,                1),
                     sl.defaults=c(  10,                 1,                1),
                     title = "Sampling Distribution Simulator",
                     but.functions= function(...){
                       relax::slider(obj.name="rerand",obj.value="Y")
                       iCLTrefresh()
                     },
                     but.names=c("Rerandomize"),
                     pos.of.panel="left")
    }
  }
}

iCLTSimPlot <- function(n,shape1,shape2,reps,incl.norm) {
  ## Create a population with a beta distribution
  x <- seq(0,0.95,0.05)
  y <- round(1000*stats::dbeta(x,shape1,shape2),0)
  x <- rep(x,y)
  # make the label
  title.popn <- substitute(paste(mu,"=",muval,", ",sigma,"=",sigmaval),
                list(muval=formatC(shape1/(shape1+shape2),format="f",digits=2),
                     sigmaval=formatC(sqrt((shape1*shape2)/(((shape1+shape2)^2)*(shape1+shape2+1))),
                                      format="f",digits=3)))
  
  ## Construct random samples from pop and their mean
  rnd.reps <- matrix(stats::rbeta(n*reps,shape1,shape2),nrow=n)
  rnd.mns <- apply(rnd.reps,2,mean)
  # compute mean and SE of sampling dist
  mn.mns <- mean(rnd.mns)
  sd.mns <- stats::sd(rnd.mns)
  title.smplng <- substitute(paste("Mean=",xbar,", SE=",se),
                             list(xbar=formatC(mn.mns,format="f",digits=2),
                                  se=formatC(sd.mns,format="f",digits=3)))
  
  ## Construct the graphic -- popn and sampling distributions
  old.par <- graphics::par(mfcol=c(1,2),mar=c(3,2,2.25,1),mgp=c(1.7,0.4,0),
                           tcl=-0.2,yaxs="i")
  # Plot the popn dist
  graphics::hist(x,freq=FALSE,breaks=seq(0,1,0.05),yaxt="n",
                 xlab="Variable (X)",ylab="",main="",right=FALSE,col="gray90")
  graphics::mtext("Population Distribution",3,line=1.3)
  graphics::mtext(title.popn,3,line=0.1)
  graphics::mtext("Frequency of Individuals",2,line=0.1)
  # Plot the sampling distribution
  # Set limits differently depending on shape1 and shape2
  if ((shape1+shape2)==2) tmp <- c(0.05,0.95)
    else tmp <- c(0.10,0.95)
  mns.hist <- graphics::hist(rnd.mns,freq=FALSE,breaks=15,
                             xlim=stats::quantile(x,tmp),
                             xlab=expression(paste("Mean ( ",bar(X)," )")),
                             yaxt="n",ylab="",main="",right=FALSE,col="gray90")
  graphics::mtext("Sampling Distribution",3,line=1.3)
  graphics::mtext(title.smplng,3,line=0.1)
  graphics::mtext("Frequency of Samples",2,line=0)
  # vertical line at mean
  graphics::lines(c(mn.mns,mn.mns),c(0,max(mns.hist$density)),col="red",lwd=2)
  # horiz line represent +/- SD of mean
  graphics::lines(c(mn.mns-sd.mns,mn.mns+sd.mns),rep(0.6*max(mns.hist$density),2),col="red",lwd=2)
  # potentially include normal distribution
  if (incl.norm) {
    norm.vals <- seq(min(mns.hist$breaks),max(mns.hist$breaks),length.out=50)
    graphics::lines(norm.vals,stats::dnorm(norm.vals,mn.mns,sd.mns),col="blue")
  }
  graphics::par(old.par)
} # end iCLTSimPlot internal function




#' @title Dynamic simulation of power calculation in 1-sample mean problems.
#' 
#' @description This function plots the null and actual distributions, highlights the rejection region and the power region, and allows the user to dynamically manipulate the effect size, standard deviation, sample size, and alpha to determine the effect of these parameters on power.
#' 
#' @details If the user is using RStudio and the \pkg{manipulate} package is installed then the dynamic graph is produced in the \dQuote{Plots} pane of RStudio.  The plot controls may be accessed through the \dQuote{gear} that is in the upper-left corner of the plot.  If the user is not using RStudio or the \pkg{manipulate} package is not installed, then an attempt is made to produce the dynamic graph with Tcl/Tk using the functions in the \pkg{relax} package.
#' 
#' @param mu0 A single numeric that is the null hypothesized mean
#' @param s.mua A single numeric that is the starting value for the actual population mean
#' @param s.sigma A single numeric that is the starting value for the actual population standard deviation
#' @param s.n A single numeric that is the starting value for the sample size
#' @param s.alpha A single numeric that is the starting value for alpha
#' @param lower.tail A single logical that indicates if the rejection region is into the lower tail or not
#' 
#' @return None, but a dynamic graphic with sliders is produced.
#' 
#' @keywords dynamic
#' 
#' @examples
#' \dontrun{
#' powerSim()
#' powerSim(lower.tail=FALSE)
#' }
#' 
#' @export
#' 
powerSim <- function(mu0=100,s.mua=95,s.sigma=10,s.n=30,s.alpha=0.05,lower.tail=TRUE) {
  ## Trying to fix "no visible bindings" problem for Check
  mua <- sigma <- n <- alpha <- NULL
  if (iCheckRStudio() & requireNamespace("manipulate",quietly=TRUE)) {
    rerand <- TRUE
    manipulate::manipulate(
      {
        if (rerand) set.seed(sample(1:10000))
        iPowerSimPlot(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail)
      },
      mua=manipulate::slider(min(mu0,s.mua)-2*s.sigma,max(mu0,s.mua)+2*s.sigma,step=1,
                             initial=s.mua,label="Actual mu"),
      sigma=manipulate::slider(1,3*s.sigma,step=1,initial=s.sigma),
      n=manipulate::slider(2,100,step=1,initial=s.n),
      alpha=manipulate::slider(0.01,0.30,step=0.01,initial=s.alpha)
    ) # end manipulate
  } else {
    ## internal referesher function
    iPowerRefresh <- function(...) {
      mua <- relax::slider(no=1)
      sigma <- relax::slider(no=2)
      n <- relax::slider(no=3)
      alpha <- relax::slider(no=4)
      iPowerSimPlot(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail)
    }
    if (iChk4Namespace("relax")) {
      relax::gslider(iPowerRefresh,prompt=TRUE,vscale=1.5,
                     sl.names=   c(             "Actual mu",   "sigma", "n", "alpha"),
                     sl.mins=    c(min(mu0,s.mua)-2*s.sigma,         1,   2,    0.01),
                     sl.maxs=    c(max(mu0,s.mua)+2*s.sigma, 3*s.sigma, 100,    0.30),
                     sl.deltas=  c(                       1,         1,   1,    0.01),
                     sl.defaults=c(                   s.mua,   s.sigma, s.n, s.alpha),
                     title = "Power Simulator",pos.of.panel="left")
    }
  }
}
  
iPowerSimPlot <- function(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail){
  ## Set up the null and alternative distributions
  # Find critical value
  SE <- sigma/sqrt(n)
  tmp <- ifelse(lower.tail,alpha,1-alpha)
  cv <- round(stats::qnorm(tmp,mu0,SE),2)
  # Create the null distribution values
  x0 <- sort(c(cv,seq(-4*SE+mu0,4*SE+mu0,by=0.01)))
  norm0 <- stats::dnorm(x0,mu0,SE)
  # create the actual distribution values
  xa <- sort(c(cv,seq(-4*SE+mua,4*SE+mua,by=0.01)))
  norma <- stats::dnorm(xa,mua,SE)

  ## Construct the graphics  
  # set commonalities
  old.par <- graphics::par(mgp=c(2,0.75,0),mfrow=c(2,1))
  xlmts <- c(min(mu0,s.mua)-1*s.sigma,max(mu0,s.mua)+1*s.sigma)
  ylmts <- c(0,stats::dnorm(0,0,s.sigma/sqrt(2*s.n)))
  # build the null distribution
  graphics::par(mar=c(2,1,2,3))
  c.region(cv,x0,norm0,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,
           shade.col="red",lbl.col="red",show.lbl=TRUE,
           xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
  graphics::mtext("Null Dist.",4,cex=1.5,line=1)
  graphics::lines(c(mu0,mu0),c(0,max(norm0)),lty=3,lwd=2,col="blue")
  graphics::abline(v=cv,lty=2,lwd=2,col="red")
  
  graphics::text(cv,0.9*ylmts[2],"Reject Ho",pos=ifelse(lower.tail,2,4),col="red")
  tmp <- substitute(paste(mu,"=",mua,", ",sigma,"=",sigmaval,", n=",n,", ",alpha,"=",alphaval),
                    list(mua=mua,sigmaval=sigma,n=n,alphaval=format(alpha,nsmall=2)))
  graphics::mtext(tmp,3,line=0.5)
  # build the actual distribution
  graphics::par(mar=c(3,1,1,3))
  c.region(cv,xa,norma,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,
           shade.col="green",lbl.col="green",show.lbl=FALSE,
           xlab="",xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
  graphics::mtext("means",1,line=1.5,cex=1.25)
  graphics::mtext("Actual Dist.",4,cex=1.5,line=1)
  graphics::lines(c(mua,mua),c(0,max(norma)),lty=3,lwd=2,col="blue")
  graphics::abline(v=cv,lty=2,lwd=2,col="red")
  graphics::abline(v=mu0,lty=3,lwd=2,col="gray")
  pwr <- round(stats::pnorm(cv,mua,SE),3)
  graphics::legend(ifelse(lower.tail,"topleft","topright"),
                   legend=paste("power=",format(pwr,nsmall=3)),bty="n")
  
  graphics::legend(ifelse(lower.tail,"topright","topleft"),
                   legend=paste("beta=",format(1-pwr,nsmall=3)),bty="n")
  graphics::par(old.par)
} # end iPowerSimPlot internal function




#' @title Dynamic simulations to demonstrate how the mean and median are computed.
#' 
#' @description This function visually illustrates how the mean \dQuote{balances} distances and the median \dQuote{balances} individuals in a random sample from a known population.  The user may plot their own data or compute a random sample from a beta distribution where the number of individuals in the sample and the shape of the population may be dynamically manipulated to determine how these attributes affect the calculation of the mean and median.
#' 
#' @details If the user is using RStudio and the \pkg{manipulate} package is installed then the dynamic graph is produced in the \dQuote{Plots} pane of RStudio.  The plot controls may be accessed through the \dQuote{gear} that is in the upper-left corner of the plot.  If the user is not using RStudio or the \pkg{manipulate} package is not installed, then an attempt is made to produce the dynamic graph with Tcl/Tk using the functions in the \pkg{relax} package.
#' 
#' @param x An optional numeric vector (of actual data) to be used in the visual.
#' @param outlier A string that indicates whether the outlier should be modeled at the maximum (\code{="max"}), minimum (\code{="min"}), or not at all \code{="none"}).  This is ignored if \code{x} is not \code{NULL}.
#' 
#' @return None, but a graphic (if \code{x} is not \code{null}) or a dynamic graphic with sliders (if \code{x} is \code{null}) is produced.
#' 
#' @keywords dynamic
#' 
#' @examples
#' \dontrun{
#' ## Examples with simulated data
#' ##   no outlier
#' meanMedian()
#' 
#' ##   outlier at minimum
#' meanMedian(outlier="min")
#' 
#' ##   no outlier
#' meanMedian(outlier="max")
#' }
#' 
#' ## Example with user-derived data
#' meanMedian(c(1:7,25))
#' 
#' @export
meanMedian <- function(x=NULL,outlier=c("none","max","min")) {
  ## Trying to fix "no visible bindings" problem for Check
  n <- shape1 <- shape2 <- NULL
  if (!is.null(x)) {
    ## data was supplied
    if (!is.vector(x) | !is.numeric(x)) stop("'x' must be a numeric vector.",call.=FALSE)
    iMMMakePlots(x)
  } else {
    ## Need to make up data
    outlier <- match.arg(outlier)
    if (iCheckRStudio() & requireNamespace("manipulate",quietly=TRUE)) {
      rerand <- TRUE
      manipulate::manipulate(
        {
          if (rerand) set.seed(sample(1:10000))
          x <- iMMMakeData(n,shape1,shape2,outlier)
          iMMMakePlots(x)
        },
        n=manipulate::slider(10,100,step=1,initial=30),
        shape1=manipulate::slider(1,10,step=1,label="Shape 1 (alpha)"),
        shape2=manipulate::slider(1,10,step=1,label="Shape 2 (beta)"),
        outlier=manipulate::picker("none","min","max"),
        rerand=manipulate::button("Rerandomize")
      ) # end manipulate
    } else {
      ## internal referesher function
      iMMBRefresh <- function(...) {
        # get random data
        x <- iMMMakeData(relax::slider(no=1),relax::slider(no=2),relax::slider(no=3))
        iMMMakePlots(x)
      } # end iMMBRefresh
      if (iChk4Namespace("relax")) {
        relax::gslider(iMMBRefresh,prompt=TRUE,vscale=1.5,hscale=1.75,
                       sl.names=   c("n", "Shape 1 (alpha)", "Shape 2 (beta)"),
                       sl.mins=    c( 10,                 1,                1),
                       sl.maxs=    c(100,                10,               10),
                       sl.deltas=  c(  1,                 1,                1),
                       sl.defaults=c( 30,                 1,                1),
                       title = "Mean vs. Median Simulator",
                       but.functions= function(...){
                         relax::slider(obj.name="rerand",obj.value="Y")
                         iMMBRefresh()
                       },
                       but.names=c("Re-Randomize"),
                       pos.of.panel="left")
      }
    }
  }
}


# Internal function to make random data from a beta distribution
iMMMakeData <- function(n,shape1,shape2,outlier) {
  x <- stats::rbeta(n,shape1,shape2)
  x <- x-min(x)           # rescale to a min of 0
  x <- x/max(x)           # and a max of 1
  if (outlier=="max") {
    x <- x/2              # rescale max to 0.5
    x[length(x)] <- 1     # but replace max with 1 to be an outlier
  } else if (outlier=="min") {
    x <- (x+1)/2          # rescale to min of 0.5 and max of 1
    x[1] <- 0             # but replace min with 0 to be an outlier
  }
  x
}

## Internal function to do the plotting
iMMMakePlots <- function(x) {
  ## Prepare the data and some useful values
  x <- x[order(x)]
  mn.x <- mean(x)
  mdn.x <- stats::median(x)
  n <- length(x)
  mdn.pos <- (n+1)/2
  ## Construct the graphic
  old.par <- graphics::par(mfcol=c(2,1))
  # Make the histogram on top
  graphics::par(mar=c(0.05,3.5,2,1),mgp=c(2,0.5,0),tcl=-0.2,xaxt="n")
  graphics::hist(x,main="",col="gray90",right=FALSE)
  graphics::abline(h=0)
  graphics::abline(v=c(mn.x,mdn.x),col=c("red","blue"),lwd=2)
  mdn.ttl <- paste("Median =",formatC(mdn.x,format="f",digits=2),ifelse(mdn.x>mn.x," *",""))
  mn.ttl <- paste("Mean =",formatC(mn.x,format="f",digits=2),ifelse(mn.x>mdn.x," *",""))
  graphics::mtext(c(mdn.ttl,mn.ttl),line=c(1,0),col=c("blue","red"))
  # Scatterlot on the bottom
  graphics::par(mar=c(3.5,3.5,0.05,1),mgp=c(2,0.5,0),tcl=-0.2,xaxt="s")
  graphics::plot(x,1:n,xlab="Quantitative Variable",ylab="Ordered Individual",
                 col.main="blue",xlim=range(pretty(range(x),n=grDevices::nclass.Sturges(x)))) 
  # Put median on the plot
  graphics::abline(v=mdn.x,col="blue",lwd=2)
  if (mdn.pos == floor(mdn.pos)) { 
    lwr <- 1:(mdn.pos-1)
    upr <- (mdn.pos+1):n
  } else { 
    lwr <- 1:floor(mdn.pos)
    upr <- (floor(mdn.pos)+1):n
  }
  # color points relative to the median
  graphics::points(x[lwr],lwr,col="black",pch=19)
  graphics::points(x[upr],upr,col="red",pch=19)  
  # Put mean on the plot
  graphics::abline(v=mn.x,col="red",lwd=2)
  # put residuals from mean on graph
  for (i in 1:n) {
    if (x[i] < mn.x) graphics::lines(c(x[i],mn.x),c(i,i),col="black",lty=3)
    else graphics::lines(c(x[i],mn.x),c(i,i),col="red",lty=3)
  }
  graphics::par(old.par)
} # end internal iMMMakePlots
