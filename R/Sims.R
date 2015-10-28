#' @title A dynamics graphic to illustrate the concept of confidence intervals for a mean.
#' 
#' @description This function demonstrates the concept of confidence regions by drawing a large number of random samples from a known normal distribution, computing a confidence interval for each sample, and ploting those confidence intervals.  Slider bars then let the user change the level of confidence, the sample size, or the type of confidence region (interval or bound) to see how that effects the confidence interval widths (margin-of-error) and capture probability.
#' 
#' @param reps A single numeric indicating the number of replicate samples to draw from the population
#' @param method A single string indicating whether to make confidence intervals using a normal (\code{="z"}) or t (\code{="t"}) distribution
#' @param mu A single numeric indicating the mean of the known normal distribution
#' @param sigma A single numeric indicating the standard deviation of the known normal distribution
#' 
#' @return None, but a dynamic graphic with slider bars is produced.
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



#' A dynamic graphic to illustrate the Central Limit Theorem.
#' 
#' A dynamic graphic to illustrate the Central Limit Theorem.  The user can change the population distribution that is sampled from and the sample size.
#' 
#' This function produces two graphics.  The left-most graphic is a histogram of the individuals in the population and the right-most graphic is a histogram of the simulated sampling distribution (i.e., means from the multiple samples).  The right-most graphic may include a normal distribution density curve if the \code{incl.norm} argument is used.
#' 
#' The two graphics are dynamically controlled by three slider bars.  The first two slider bars control the shape parameters of the beta distribution used to model the population distribution that will be sampled from.  The beta distribution allows for a wide variety of shapes for the population distribution.  The last slider bar controls the size of each sample taken from the population distribution.  The slider bars can be used to detect how changes in the shape of the population and the size of the sample effect the shape, center, and dispersion of the sampling distribution.
#' 
#' @param reps Number of samples to take from the population distribution and means to calculate
#' @param incl.norm A logical indicating whether a normal density curve should be superimposed on the sampling distribution
#' 
#' @return None, but a dynamic graphic with slider bars will be produced.
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
  if (iCheckRStudio() & requireNamespace("manipulate",quietly=TRUE)) {
    rerand <- TRUE
    manipulate::manipulate(
      {
        if (rerand) set.seed(sample(1:10000))
        iCLTSimPlot(n,sh1,sh2,reps,incl.norm)
      },
      n=manipulate::slider(10,100,step=5,initial=10),
      sh1=manipulate::slider(1,20,step=1,label="Shape 1 (alpha)"),
      sh2=manipulate::slider(1,20,step=1,label="Shape 2 (beta)"),
      rerand=manipulate::button("Rerandomize")
    ) # end manipulate
  } else {
    ## internal referesher function
    iCLTrefresh <- function(...) {
      n <- relax::slider(no=1)
      sh1 <- relax::slider(no=2)
      sh2 <- relax::slider(no=3)
      iCLTSimPlot(n,sh1,sh2,reps,incl.norm)
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

iCLTSimPlot <- function(n,sh1,sh2,reps,incl.norm) {
  ## Create a population with a beta distribution
  x <- seq(0,0.95,0.05)
  y <- round(1000*stats::dbeta(x,sh1,sh2),0)
  x <- rep(x,y)
  # make the label
  title.popn <- substitute(paste(mu,"=",muval,", ",sigma,"=",sigmaval),
                list(muval=formatC(sh1/(sh1+sh2),format="f",digits=2),
                     sigmaval=formatC(sqrt((sh1*sh2)/(((sh1+sh2)^2)*(sh1+sh2+1))),
                                      format="f",digits=3)))
  
  ## Construct random samples from pop and their mean
  rnd.reps <- matrix(stats::rbeta(n*reps,sh1,sh2),nrow=n)
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
  # Set limits differently depending on sh1 and sh2
  if ((sh1+sh2)==2) tmp <- c(0.05,0.95)
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




#' Dynamic simulation of power calculation in 1-sample mean problems.
#' 
#' This function plots the null and actual distributions, highlights the rejection region and the power region, and allows the user to manipulate the effect size, standard deviation, sample size, and alpha to determine the effect on power.
#' 
#' @param mu0 A single numeric that is the null hypothesized mean.
#' @param s.mua A single numeric that is the starting value for the actual population mean.
#' @param s.sigma A single numeric that is the starting value for the actual population standard deviation.
#' @param s.n A single numeric that is the starting value for the sample size.
#' @param s.alpha A single numeric that is the starting value for alpha.
#' @param lower.tail A single logical indicating if the rejection region is into the lower tail or not.
#' 
#' @return None, but a dynamic graphic with slider bars is produced.
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
  iPowerRefresh <- function(...) {
    mua <- relax::slider(no=1)
    sigma <- relax::slider(no=2)
    n <- relax::slider(no=3)
    alpha <- relax::slider(no=4)
    iPowerSimPlot(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail)
  }
  
  # Start of main function
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

iPowerSimPlot <- function(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail){
  old.par <- graphics::par(mgp=c(2,0.75,0),mfrow=c(2,1)); on.exit(graphics::par(old.par))
  SE <- sigma/sqrt(n)
  xlmts <- c(min(mu0,s.mua)-1*s.sigma,max(mu0,s.mua)+1*s.sigma)
  ylmts <- c(0,stats::dnorm(0,0,s.sigma/sqrt(2*s.n)))
  # Find critical value
  ifelse(lower.tail, cv <- stats::qnorm(alpha,mu0,SE), cv <- stats::qnorm(1-alpha,mu0,SE))
  cv <- round(cv,2)
  # Create the null distribution values
  x0 <- sort(c(cv,seq(-4*SE+mu0,4*SE+mu0,by=0.01)))
  norm0 <- stats::dnorm(x0,mu0,SE)
  # create the actual distribution values
  xa <- sort(c(cv,seq(-4*SE+mua,4*SE+mua,by=0.01)))
  norma <- stats::dnorm(xa,mua,SE)
  graphics::par(mar=c(2,1,2,3))
  c.region(cv,x0,norm0,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,
           shade.col="red",lbl.col="red",show.lbl=TRUE,
           xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
  graphics::mtext("Null Dist.",4,cex=1.5,line=1)
  graphics::lines(c(mu0,mu0),c(0,max(norm0)),lty=3,lwd=2,col="blue")
  graphics::abline(v=cv,lty=2,lwd=2,col="red")
  ifelse(lower.tail,pos <- 2, pos <- 4)
  graphics::text(cv,0.9*ylmts[2],"Reject Ho",pos=pos,col="red")
  graphics::mtext(paste("mu=",mua,", sigma=",sigma,", n=",n,", alpha=",
                        format(alpha,nsmall=2)),3,line=0.5)
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
  ifelse(lower.tail, pos <- "topleft", pos <- "topright")
  graphics::legend(pos,legend=paste("power=",format(pwr,nsmall=3)),bty="n")
  ifelse(lower.tail, pos <- "topright", pos <- "topleft")
  graphics::legend(pos,legend=paste("beta=",format(1-pwr,nsmall=3)),bty="n")    
} # end iPowerSimPlot internal function
