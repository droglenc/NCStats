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
  if (iCheckRStudio()) {
    if (iChk4Namespace("manipulate")) {
      rerand <- TRUE
      manipulate::manipulate(
        {
          if (rerand) set.seed(sample(1:10000))
          iCISimPlot(n,conf.level,alternative,reps,method,mu,sigma)
        },
        n=manipulate::slider(10,100,step=5,initial=10,label="Sample Size (n)"),
        conf.level=manipulate::picker(0.99,0.95,0.90,0.80,initial=0.95,label="Confidence Level"),
        alternative=manipulate::picker("two.sided","less","greater",label="Alternative Hypothesis"),
        rerand=manipulate::button("Rerandomize")
      ) # end manipulate
    }
  } else { ## use relax and Tcl/Tk
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
             crit <- stats::qnorm(conf)
             zstar <- mu-crit*sigma/sqrt(n)
             uci <- rnd.mns+crit*SE
             lci <- rep(xr[1],reps)
           },
           greater = {
             crit <- stats::qnorm(conf)
             zstar <- mu+crit*sigma/sqrt(n)
             lci <- rnd.mns-crit*SE
             uci <- rep(xr[2],reps)
           }, 
           two.sided = {
             crit <- stats::qnorm(0.5+conf/2)
             zstar <- mu+c(-1,1)*crit*sigma/sqrt(n)
             lci <- rnd.mns-crit*SE
             uci <- rnd.mns+crit*SE
           } )
    me <- crit*sigma/sqrt(n)
  } else {
    SE = apply(rnd.reps,2,function(x) stats::sd(x)/sqrt(length(x)))
    switch(alternative,
           less = {
             crit <- stats::qt(conf,df=n-1)
             uci <- rnd.mns+crit*SE
             lci <- rep(min(xr),reps)
           },
           greater = {
             crit <- stats::qt(conf,df=n-1)
             lci <- rnd.mns-crit*SE
             uci <- rep(max(xr),reps)
           }, 
           two.sided = {
             crit <- stats::qt(0.5+conf/2,df=n-1)
             lci <- rnd.mns-crit*SE
             uci <- rnd.mns+crit*SE
           }  )
    me <- mean(crit*SE)
  }
  # Identify if a CI contains mu (black) or not (red)
  colr <- ifelse(((lci>mu) | (uci<mu)),"red","black")
  # Percent CIs contained mu
  hit <- paste0(formatC(100*length(colr[colr=="black"])/reps,format="f",digits=0))
  
  ## Construct the graphic
  # Some preparations
  old.par <- graphics::par(mar=c(3,1,2,1),mgp=c(2,0.4,0),tcl=-0.2)
  xlbl <- expression(paste("Sample Mean ( ",bar(X)," )"))
  tmp <- ifelse(method=="Z","","avg ")
  title <- substitute(paste(hit,"% contain ",mu," (=",muval,"), ",tmp,
                            "m.e.=",me,),
                      list(hit=hit,tmp=tmp,me=formatC(me,format="f",digits=3),muval=mu))
  # make skeleton plot
  graphics::plot(lci,seq(1,reps),type="n",yaxt="n",xlim=xr,ylim=c(-reps*0.01,1.01*reps),yaxs="i",
                 xlab=xlbl,ylab="",main=title)
  # For z method only, put transparent green vertical lines at crit values of popn.
  if (method=="Z") {
    tmp <- graphics::par("usr")
    ys <- c(tmp[3],0.995*tmp[4],0.995*tmp[4],tmp[3])
    clr <- FSA::col2rgbt("black",0.05)
    switch(alternative,
           less = { graphics::polygon(c(rep(zstar,2),tmp[2],tmp[2]),ys,col=clr,border=NA) },
           greater = { graphics::polygon(c(rep(zstar,2),tmp[1],tmp[1]),ys,col=clr,border=NA) }, 
           two.sided = { graphics::polygon(rep(zstar,each=2),ys,col=clr,border=NA) }  )
  }
  # Put blue vertical line and label at mu
  graphics::lines(c(mu,mu),c(-0.1,1.2*reps),col="gray50",lwd=3)
  for (i in 1:reps) {
    # Put CIs on the plot
    graphics::lines(c(lci[i],uci[i]),rep(i,2),col=colr[i],lwd=1)
    # Put means on the plot
    graphics::points(rnd.mns[i],i,pch=19,col=FSA::col2rgbt(colr[i],0.7),cex=0.5)
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
cltSim <- function(reps=5000,incl.norm=FALSE) {
  ## Trying to fix "no visible bindings" problem for Check
  n <- shape1 <- shape2 <- NULL
  if (iCheckRStudio()) {
    if (iChk4Namespace("manipulate")) {
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
    }
  } else {  ## use refresh and Tcl/Tk
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
powerSim <- function(mu0=100,s.mua=95,s.sigma=10,s.n=30,s.alpha=0.05,lower.tail=TRUE) {
  ## Trying to fix "no visible bindings" problem for Check
  mua <- sigma <- n <- alpha <- NULL
  if (iCheckRStudio()) {
    if (iChk4Namespace("manipulate")) {
      rerand <- TRUE
      manipulate::manipulate(
        {
          if (rerand) set.seed(sample(1:10000))
          iPowerSimPlot(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail)
        },
        alpha=manipulate::slider(0.01,0.30,step=0.01,initial=s.alpha),
        n=manipulate::slider(10,100,step=1,initial=s.n),
        sigma=manipulate::slider(floor(s.sigma/3),ceiling(2*s.sigma),step=1,initial=s.sigma),
        mua=manipulate::slider(mu0-1.5*s.sigma,mu0+1.5*s.sigma,step=1,
                               initial=s.mua,label="Actual mu"),
        lower.tail=manipulate::checkbox(TRUE,"Ha is less than?")
      ) # end manipulate
    }
  } else { ## use relax and Tcl/Tk
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
                     sl.names=   rev(c(    "Actual mu",   "sigma", "n", "alpha")),
                     sl.mins=    rev(c(mu0-1.5*s.sigma,         1,   2,    0.01)),
                     sl.maxs=    rev(c(mu0+1.5*s.sigma, 3*s.sigma, 100,    0.30)),
                     sl.deltas=  rev(c(              1,         1,   1,    0.01)),
                     sl.defaults=rev(c(          s.mua,   s.sigma, s.n, s.alpha)),
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
  old.par <- graphics::par(mgp=c(1,0.3,0),tcl=-0.2,mfrow=c(2,1))
  xlmts <- c(mu0-2*s.sigma,mu0+2*s.sigma)
  ylmts <- c(0,1.25*stats::dnorm(0,0,s.sigma/sqrt(2*s.n)))
  # build the null distribution
  graphics::par(mar=c(2,1,2,3))
  c.region(cv,x0,norm0,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,
           shade.col="red",lbl.col="red",show.lbl=TRUE,
           xlim=xlmts,ylim=ylmts,yaxt="n",ylab="",xlab="")
  graphics::mtext("Null Dist.",4,cex=1.5,line=1)
  graphics::lines(c(mu0,mu0),c(0,max(norm0)),lty=3,lwd=2,col="blue")
  graphics::abline(v=cv,lty=2,lwd=2,col="red")
  
  graphics::text(cv,0.9*ylmts[2],"Reject Ho",pos=ifelse(lower.tail,2,4),col="red")
  graphics::text(cv,0.9*ylmts[2],"DNR Ho",pos=ifelse(!lower.tail,2,4))
  tmp <- substitute(paste(mu,"=",mua,", ",sigma,"=",sigmaval,", n=",n,", ",alpha,"=",alphaval),
                    list(mua=mua,sigmaval=sigma,n=n,alphaval=format(alpha,nsmall=2)))
  graphics::mtext(tmp,3,line=0.5)
  # build the actual distribution
  graphics::par(mar=c(3,1,1,3))
  c.region(cv,xa,norma,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,
           shade.col="green",show.lbl=FALSE,
           xlab="",xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
  c.region(cv,xa,norma,!lower.tail,area=NULL,plot=FALSE,add=TRUE,show.ans=FALSE,
           shade.col="tomato",show.lbl=FALSE,
           xlab="",xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
  xlbl <- expression(paste("Sample Mean ( ",bar(X)," )"))
  graphics::mtext(xlbl,1,line=1.8,cex=1.10)
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
    if (iCheckRStudio()) {
      if (iChk4Namespace("manipulate")) {
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
      }
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




#' @title Dynamic simulations to demonstrate how the correlation is computed.
#' 
#' @description This function simulates bivariate data with a known correlation coefficient.  A crosshairs at the bivariate means and color coding of the product of standardized values helps illustrate how the concept of the correlation.  Optionally, rectangles from each point to the bivariate mean can be include to demonstrate how the correlation sums these areas (concept from \code{\link[TeachingDemos]{cor.rect.plot}}).
#' 
#' @details Requires RStudio and the \pkg{manipulate} package to be installed.  The dynamic graph is produced in the \dQuote{Plots} pane of RStudio.  The plot controls may be accessed through the \dQuote{gear} that is in the upper-left corner of the plot.
#' 
#' @return None, but a a dynamic graphic with sliders is produced.
#' 
#' @keywords dynamic
#' 
#' @examples
#' \dontrun{
#' corrSim()
#' }
#' 
#' @export
corrSim <- function() {
  if (iChk4Namespace("manipulate")) {
    ## Trying to hangle global binding issue
    r <- n <- rectangles <- NULL
    rerand <- TRUE
    manipulate::manipulate(
      {
        if (rerand) set.seed(sample(1:10000))
        iPlotCorrData(r,n,rectangles)
      },
      r=manipulate::slider(-1,1,step=0.05,initial=0),
      n=manipulate::slider(20,300,step=10,initial=100),
      rectangles=manipulate::checkbox(FALSE,label="Show Rectangles"),
      rerand=manipulate::button("Rerandomize")
    ) # end manipulate
  }  
}

# Internal function to produce the plot
iPlotCorrData <- function(r,n,rects,trans=ifelse(n<100,0.1,0.05)) {
  old.par <- graphics::par(mar=c(1,1.5,2,1.5),mgp=c(2,0.4,0),
                           tcl=-0.2,xaxt="n",yaxt="n")
  # constructs data
  res <- as.data.frame(MASS::mvrnorm(n,Sigma=matrix(c(1,r,r,1),nrow=2),
                                     mu=c(0,0),empirical=TRUE))
  names(res) <- c("X","Y")
  # finds those with positive products
  res <- res[order(res$X^2+res$Y^2,decreasing=TRUE),]
  w <- res$X*res$Y>0
  # makes base graphic
  rng <- c(-max(abs(res)),max(abs(res)))
  graphics::plot(0,xlim=rng,ylim=rng,col="white",
                 main=paste("r =",formatC(r,format="f",digits=2)))
  graphics::abline(h=0,col="gray80",lty=2,lwd=3)
  graphics::abline(v=0,col="gray80",lty=2,lwd=3)
  # add rectangles if asked for
  if (rects) {
    if (all(w)) {
      graphics::rect(0,0,res$X,res$Y,col=col2rgbt("red",trans),border=NA)
    } else if (all(!w)) {
      graphics::rect(0,0,res$X,res$Y,col=col2rgbt("blue",trans),border=NA)
    } else {
      graphics::rect(0,0,res$X[w],res$Y[w],col=col2rgbt("red",trans),border=NA)
      graphics::rect(0,0,res$X[!w],res$Y[!w],col=col2rgbt("blue",trans),border=NA)
    }
  }
  # add the points (color coded)
  graphics::points(Y~X,data=res[w,],pch=19,col="red")
  graphics::points(Y~X,data=res[!w,],pch=19,col="blue")
  graphics::par(old.par)
}



#' @title Plot targets and histograms for illustrating accuracy and precision.
#' 
#' @description Plot \dQuote{targets} and histograms for illustrating accuracy and precision.  The user can choose the number of points to sample.
#' 
#' @param n Number of random points to put on each target and histogram.
#' @param r Number of rings (i.e., circles) on the target.
#' @param pts.col String indicating color to use for plotting points on the target.
#' @param pts.trans Value between 0 and 1 indicating the transparency to use for plotting points on the target.  The inverse of this value indicates how many points must be overplotted before the full color is shown.
#' @param pts.cex Character expansion amount for points on target.
#' @param mns.col String indicating color to use for line marking the mean on the targets and the histograms.
#' @param mns.pch Value indicating the plotting character to use for the mean on the targets.
#' @param mns.lwd Value indicating the line width to use use for the mean on the histograms.
#' 
#' @return None.  A plot is produced.
#' 
#' @keywords dynamic
#' 
#' @examples
#' accuracyPrecision()
#' accuracyPrecision(100,5)
#' 
#' @export
accuracyPrecision <- function(n=50,r=4,pts.col="red",pts.trans=0.6,pts.cex=1,
                              mns.col="blue",mns.pch=3,mns.lwd=2) {
  old.par <- graphics::par(mfcol=c(4,2),mar=c(1.5,3,1.5,0)); on.exit(graphics::par(old.par))
  # Standard deviation for precise situations
  ps <- 1/2                                                         
  # Standard deviation for imprecise situations
  is <- r/3                                                         
  ap.x <- stats::rnorm(n,0,ps)
  ai.x <- stats::rnorm(n,0,is)
  ip.x <- stats::rnorm(n,1.5,ps)
  ii.x <- stats::rnorm(n,1.5,is)
  
  iMakeTarget(r,ap.x,stats::rnorm(n,0,ps),"Accurate","Precise",
              pts.cex,pts.col,pts.trans,mns.col,mns.pch) 
  iMakeTarget(r,ai.x,stats::rnorm(n,0,is),"Accurate","Imprecise",
              pts.cex,pts.col,pts.trans,mns.col,mns.pch)
  iMakeTarget(r,ip.x,stats::rnorm(n,-1.5,ps),"Inaccurate","Precise",
              pts.cex,pts.col,pts.trans,mns.col,mns.pch)
  iMakeTarget(r,ii.x,stats::rnorm(n,-1.5,is),"Inaccurate","Imprecise",
              pts.cex,pts.col,pts.trans,mns.col,mns.pch)
  
  tmp <- c(graphics::hist(ap.x,plot=FALSE)$counts,
           graphics::hist(ai.x,plot=FALSE)$counts,
           graphics::hist(ip.x,plot=FALSE)$counts,
           graphics::hist(ii.x,plot=FALSE)$counts)
  ylmts <- c(0,max(tmp))
  tmp <- c(graphics::hist(ap.x,plot=FALSE)$breaks,
           graphics::hist(ai.x,plot=FALSE)$breaks,
           graphics::hist(ip.x,plot=FALSE)$breaks,
           graphics::hist(ii.x,plot=FALSE)$breaks) 
  minbrk <- min(tmp)
  maxbrk <- max(tmp)
  brks <- seq(minbrk,maxbrk,by=0.5)
  graphics::par(mar=c(2,1,1,1))
  # Dist - Accurate and Precise
  iMakeDist(ap.x,ylmts,brks,pts.col,pts.trans,mns.col,mns.lwd)       
  # Dist - Accurate and Imprecise
  iMakeDist(ai.x,ylmts,brks,pts.col,pts.trans,mns.col,mns.lwd)
  # Dist - Inaccurate and Precise
  iMakeDist(ip.x,ylmts,brks,pts.col,pts.trans,mns.col,mns.lwd)
  # Dist - Inaccurate and Imprecise
  iMakeDist(ii.x,ylmts,brks,pts.col,pts.trans,mns.col,mns.lwd)
}



## Internal function to make the bullseye target graph with random points on it.
iMakeTarget<-function(r,px,py,t1,t2,pts.cex,pts.col,pts.trans,mns.col,mns.pch){  
  # Make the schematic plot with a bullseye in the middle
  graphics::plot(0,0,xlim=c(-r,r),ylim=c(-r,r),type="p",pch=19,
                 axes=FALSE,yaxt="n",xaxt="n",xlab="",ylab="")
  # Put on circles with integer radii to r
  for (i in 1:r) {
    # Find x-coords of circle
    cx <- seq(-i,i,by=0.005)
    # Find y-coords for top-half of the circle
    cyt <- sqrt(i^2-cx^2)
    # Find y-coords for bottom-half of the circle
    cyb <- -sqrt(i^2-cx^2)
    # Inner circles are ligher grey
    clr <- ifelse(i==r,"grey30","grey80")
    # Draw upper half of circle
    graphics::lines(cx,cyt,lwd=2,col=clr)
    # Draw lower half of circle
    graphics::lines(cx,cyb,lwd=2,col=clr)
  }
  pts.col <- FSA::col2rgbt(pts.col,pts.trans)
  # Put random points on graph in red
  graphics::points(px,py,pch=19,col=pts.col,cex=pts.cex)                                
  # Put a blue plus at center of points (mx,my)
  graphics::points(mean(px),mean(py),pch=mns.pch,col=mns.col,cex=round(1.5*pts.cex,1))  
  graphics::mtext(t1,2)         # Label graphs
  graphics::mtext(t2,2,1.5)
} ## end internal iMakeTarget

## Internal function to make the histogram of the random points.
iMakeDist <-function(px,ylmts,brks,pts.col,pts.trans,mns.col,mns.lwd) {  
  pts.col <- FSA::col2rgbt(pts.col,pts.trans)
  # Plot the histogram
  graphics::hist(px,main="",breaks=brks,axes=FALSE,ylim=ylmts,col=pts.col)
  # Put blue vertical line at sample mean
  graphics::lines(c(mean(px),mean(px)),ylmts,lwd=mns.lwd,col=mns.col)
  # Put black tick at true mean
  graphics::lines(c(0,0),c(-0.25,0),lwd=2,col="black")
  graphics::text(0,-0.25,"T",pos=1,xpd=TRUE,cex=1.5)
} ## end internal iMakeDist()




#' @title Shows the steps in the manual calculation of the standard deviation.
#' 
#' @description Shows the steps in the manual calculation of the standard deviation.
#' 
#' @aliases sdCalc print.sdCalc
#' 
#' @param x A numeric vector
#' @param digits A numeric indicating the number of decimals to round the numeric summaries to.  If left at \code{NULL} (default) then the number of digits will be obtained from \code{getOption('digits')}
#' @param \dots Other arguments to the generic \code{print} functions (not currently used)
#' 
#' @return A list containing the sample size (\code{n}), sample mean (\code{mean}), data.frame of intermediate calculations (\code{tbl}), and number of digits to print (\code{digits))}. 
#' 
#' @note This function shows a table of intermediate output in the calculation of the standard deviation.  Used purely to demonstrate the hand-calculation of the standard deviation.  Use \code{\link[stats]{sd}} to actually compute the standard deviation.
#' 
#' @seealso \code{\link[stats]{sd}}
#' 
#' @keywords misc
#' 
#' @examples
#' ## Numeric vector
#' y <- runif(8)
#' # typical computation
#' sd(y)   
#' # this function           
#' sdCalc(y)            
#' # this function, controlling the number of digits
#' sdCalc(y,digits=4)
#' 
#' @rdname sdCalc
#' @export
sdCalc <- function(x,digits=getOption("digits")) {
  # make sure the vector is numeric
  if (!is.numeric(x)) stop("x must be numeric to compute the sd.",call.=FALSE)
  # remove missing values
  x1 <- x[!is.na(x)]
  # calculate parts
  n <- length(x1)
  xbar <- mean(x1)
  diffs <- x1-mean(x1)
  diffs.sq <- diffs^2
  df <- data.frame(x=x1,diffs,diffs.sq)
  df[nrow(df)+1,] <- c(sum(df[,1]),sum(df[,2]),sum(df[,3]))
  rownames(df)[nrow(df)] <- "sum"
  res <- list(n=n,mean=xbar,tbl=df,digits=digits)
  class(res) <- "sdCalc"
  res
}

#' @rdname sdCalc
#' @method print sdCalc
#' @export
print.sdCalc <- function(x,...) {
  cat("Demonstration of parts of a std. dev. calculation.\n\n")
  print(round(x$tbl,x$digits))
  cat("\nMean = x-bar =",round(x$tbl[nrow(x$tbl),"x"],x$digits),"/",x$n,"=",round(x$mean,x$digits),"\n")
  vrnc <- x$tbl[nrow(x$tbl),"diffs.sq"]/(x$n-1)
  cat("\nVariance = s^2 =",round(x$tbl[nrow(x$tbl),"diffs.sq"],x$digits),"/",x$n-1,"=",round(vrnc,x$digits),"\n")
  cat("\nStd. Dev = s = sqrt(",round(vrnc,x$digits),") = ",round(sqrt(vrnc),x$digits),"\n",sep="")
}


#' @title Shows the steps in the manual calculation of the median and IQR.
#' 
#' @description Shows the steps in the manual calculation of the median and IQR.
#' 
#' @aliases iqrCalc print.iqrCalc
#' 
#' @param x A numeric vector
#' @param mdnInBoth A logical that indicates whether the median should be placed into both halves or not (DEFAULT) when computing the IQR with odd n.
#' @param \dots Other arguments to the generic \code{print} functions (not currently used)
#' 
#' @return A list containing the sample size (\code{n}); the sample Q1, median, and Q3 in the name vector \code{vals}; the positions of the sample Q1, median, and Q3 in the named vector \code{pos}; the ordered data (\code{x}); and the ordered lower- (\code{lwr}) and upper-halves (\code{upr}) of the data.
#' 
#' @note This function shows the ordered data with the median shown and the ordered lower- and upper-halves of data with the Q1 and Q3 values shown.  This function puts the median into both halves of the data to compute Q1 and Q3.
#' 
#' @seealso \code{\link{sdCalc}}
#' 
#' @keywords misc
#' 
#' @examples
#' ## Simple examples
#' iqrCalc(1:7)
#' iqrCalc(1:7,mdnInBoth=TRUE)
#' iqrCalc(1:8)
#' iqrCalc(1:8,mdnInBoth=TRUE)
#' iqrCalc(1:9)
#' iqrCalc(1:9,mdnInBoth=TRUE)
#' iqrCalc(1:10)
#' iqrCalc(1:10,mdnInBoth=TRUE)
#' 
#' ## Somewhat more realistic
#' iqrCalc(sample.int(99,11,replace=TRUE))
#' iqrCalc(sample.int(99,12,replace=TRUE))
#' iqrCalc(sample.int(99,13,replace=TRUE))
#' iqrCalc(sample.int(99,14,replace=TRUE))
#' 
#' @rdname iqrCalc
#' @export
iqrCalc <- function(x,mdnInBoth=FALSE) {
  # make sure the vector is numeric
  if (!is.numeric(x)) stop("x must be numeric to compute the sd.",call.=FALSE)
  # remove missing values
  x1 <- x[!is.na(x)]
  # sort data
  x1 <- x1[order(x1)]
  # calculate parts
  n <- length(x1)
  # find median and split into the two parts
  mdn.pos <- floor((n+1)/2)
  if (is.odd(n)) {
    mdn <- x1[mdn.pos]
    adj <- ifelse(mdnInBoth,0,1)
    x1.lwr <- x1[1:(mdn.pos-adj)]
    x1.upr <- x1[(mdn.pos+adj):n]
  } else {
    mdn <- mean(x1[c(mdn.pos,mdn.pos+1)])
    x1.lwr <- x1[1:mdn.pos]
    x1.upr <- x1[(mdn.pos+1):n]
  }
  # find Q1 and Q3
  n2 <- length(x1.lwr)
  q1.pos <- q3.pos <- floor((n2+1)/2)
  if (is.odd(n2)) {
    q1 <- x1.lwr[q1.pos]
    q3 <- x1.upr[q3.pos]
  } else {
    q1 <- mean(x1.lwr[c(q1.pos,q1.pos+1)])
    q3 <- mean(x1.upr[c(q1.pos,q1.pos+1)])
  }
  # return results as a list
  res <- list(n=n,vals=c(Q1=q1,median=mdn,Q3=q3),
              pos=c(Q1=q1.pos,median=mdn.pos,Q3=q3.pos),
              x=x1,lwr=x1.lwr,upr=x1.upr,mdnInBoth=mdnInBoth)
  class(res) <- "iqrCalc"
  res
}

#' @rdname iqrCalc
#' @method print iqrCalc
#' @export
print.iqrCalc <- function(x,...) {
  # median
  tmp <- paste(x$x[1:(x$pos["median"]-1)],collapse=" ")
  if (FSA::is.odd(x$n)) {
    cat("Median (=",x$vals["median"],") is the value in position ",
        x$pos["median"],".\n",sep="")
    tmp <- paste0(tmp," [",x$x[x$pos["median"]],"] ")
    tmp <- paste0(tmp,paste(x$x[(x$pos["median"]+1):x$n],collapse=" "))
  } else {
    cat("Median (=",x$vals["median"],") is the average of values in positions ",
        x$pos["median"]," and ",x$pos["median"]+1,".\n",sep="")
    tmp <- paste0(tmp," [",x$x[x$pos["median"]]," ",x$x[x$pos["median"]+1],"] ")
    tmp <- paste0(tmp,paste(x$x[(x$pos["median"]+2):x$n],collapse=" "))
  }
  cat("  ",tmp,"\n\n",sep="")
  # Q1
  n2 <- length(x$lwr)
  tmp <- paste(x$lwr[1:(x$pos["Q1"]-1)],collapse=" ")
  if (FSA::is.odd(n2)) {
    cat("Q1 (=",x$vals["Q1"],") is the value in position ",
        x$pos["Q1"]," of the lower half.\n",sep="")
    tmp <- paste0(tmp," [",x$lwr[x$pos["Q1"]],"] ")
    tmp <- paste0(tmp,paste(x$lwr[(x$pos["Q1"]+1):n2],collapse=" "))
  } else {
    cat("Q1 (=",x$vals["Q1"],") is average of values in positions ",x$pos["Q1"],
        " and ",x$pos["Q1"]+1," of the lower half.\n",sep="")
    tmp <- paste0(tmp," [",x$lwr[x$pos["Q1"]]," ",x$lwr[x$pos["Q1"]+1],"] ")
    tmp <- paste0(tmp,paste(x$lwr[(x$pos["Q1"]+2):n2],collapse=" "))
  }
  cat("  ",tmp,"\n\n",sep="")
  # q3
  tmp <- paste(x$upr[1:(x$pos["Q3"]-1)],collapse=" ")
  if (FSA::is.odd(n2)) {
    cat("Q3 (=",x$vals["Q3"],") is the value in position ",x$pos["Q3"]," of the upper half.\n",sep="")
    tmp <- paste0(tmp," [",x$upr[x$pos["Q3"]],"] ")
    tmp <- paste0(tmp,paste(x$upr[(x$pos["Q3"]+1):n2],collapse=" "))
  } else {
    cat("Q3 (=",x$vals["Q3"],") is average of values in positions ",x$pos["Q3"],
        " and ",x$pos["Q3"]+1," of the upper half.\n",sep="")
    tmp <- paste0(tmp," [",x$upr[x$pos["Q3"]]," ",x$upr[x$pos["Q3"]+1],"] ")
    tmp <- paste0(tmp,paste(x$upr[(x$pos["Q3"]+2):n2],collapse=" "))
  }
  cat("  ",tmp,"\n\n",sep="")
  if (FSA::is.odd(x$n)) {
    if (x$mdnInBoth) {
      cat("**Note that the median (=",x$vals["median"],") IS in both halves.\n\n",sep="")
    } else {
      cat("**Note that the median (=",x$vals["median"],") is NOT in both halves.\n\n",sep="")
    }
  }
}
