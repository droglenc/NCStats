#'A dynamics graphic to illustrate the concept of confidence intervals for a mean.
#'
#'This function demonstrates the concept of confidence regions by drawing a large
#'number of random samples from a known normal distribution, computing a 
#'confidence interval for each sample, and ploting those confidence intervals.
#'Slider bars then let the user change the level of confidence, the sample size,
#'or the type of confidence region (interval or bound) to see how that effects
#'the confidence interval widths (margin-of-error) and capture probability.
#'
#'@param reps A single numeric indicating the number of replicate samples to draw from
#'the population.
#'@param method A single string indicating whether to make confidence intervals using
#'a normal (\code{="z"}) or t (\code{="t"}) distribution.
#'@param mu A single numeric indicating the mean of the known normal distribution.
#'@param sigma A single numeric indicating the standard deviation of the known normal
#'distribution.
#'@return None, but a dynamic graphic with slider bars is produced.
#'@keywords misc dynamic
#'@examples
#'if (interactive()) {
#'
#'# Default using normal theory for confidence regions
#'ciSim()
#'
#'# Using t-distribution theory for confidence regions
#'ciSim(method="t")
#'
#'}
#'
#'@export
ciSim <- function(reps=100,method=c("z","Z","t","T"),mu=100,sigma=10) {
  refresh <- function(...) {
    n <- relax::slider(no=1)
    conf <- relax::slider(no=2)
    tail <- relax::slider(no=3)
    tail <- ifelse(tail==-1,"less.than",ifelse(tail==0,"two.sided","greater.than")) # change tail number to character                                                         
    ciSimPlot(n,conf,tail,reps,method,mu,sigma)
  } # end refresh internal function

  ciSimPlot <- function(n,conf,tail,reps,method,mu,sigma,...) {
    xr <- mu+c(-5,5)*sigma/sqrt(10)                     # Sets x range of plot, allows to see change in width of CIs
    rnd.reps <- matrix(rnorm(n*reps,mu,sigma),nrow=n)   # random samples from popn
    rnd.mns <- apply(rnd.reps,2,mean)                   # compute means of random samples
    switch(method,
           Z=, z= {                                     # Make CIs for each resample for Z method
             SE = rep(sigma/sqrt(n),reps)
             switch(tail,
                    less.than = {
                      crit <- qnorm(conf)
                      uci <- rnd.mns+crit*SE
                      lci <- rep(min(xr),reps)
                    },
                    greater.than = {
                      crit <- qnorm(conf)
                      lci <- rnd.mns-crit*SE
                      uci <- rep(max(xr),reps)
                    }, 
                    two.sided = {
                      crit <- qnorm(1-(1-conf)/2)
                      lci <- rnd.mns-crit*SE
                      uci <- rnd.mns+crit*SE   
                    }
             ) # end tail switch within Z method
           }, # end Z method
           T=, t= { # Make CIs for each resample for t method
             SE = apply(rnd.reps,2,sd)/sqrt(n)
             switch(tail,
                    less.than = {
                      crit <- qt(conf,df=n-1)
                      uci <- rnd.mns+crit*SE
                      lci <- rep(min(xr),reps)
                    },
                    greater.than = {
                      crit <- qt(conf,df=n-1)
                      lci <- rnd.mns-crit*SE
                      uci <- rep(max(xr),reps)
                    }, 
                    two.sided = {
                      crit <- qt(1-(1-conf)/2,df=n-1)
                      lci <- rnd.mns-crit*SE
                      uci <- rnd.mns+crit*SE
                    }
             ) # end tail switch within t method
           } # end t method
    ) # end method switch
    old.par <- par(mar=c(3,1,3.5,1), mgp=c(2,0.4,0), tcl=-0.2); on.exit(par(old.par))
    plot(lci,seq(1,reps),type="n",yaxt="n",xlim=xr,xlab=expression(paste("Sample Mean( ",bar(X)," )")),ylab="",main="")  # make skeleton plot
    lines(c(mu,mu),c(-0.1,1.2*reps),lwd=3,lty=3,col="blue")                                   # Put blue vertical line at mu
    text(mu,-1.5,expression(mu),cex=1.25,col="blue")                                          # lable mu line
    if (method=="Z" | method=="z") {                                                          # Put green vertical lines at crit values of popn
      switch(tail,                                                                            #    only for Z method
             less.than = {     abline(v=mu-crit*sigma/sqrt(n),col="green",lty=3,lwd=2) },
             greater.than = {  abline(v=mu+crit*sigma/sqrt(n),col="green",lty=3,lwd=2) }, 
             two.sided = {     abline(v=mu+c(-crit,crit)*sigma/sqrt(n),col="green",lty=3,lwd=2) }
      ) # end tail switch
    }
    colr <- ifelse(((lci>mu) | (uci<mu)),"red","black")                                       # Identify if a CI contains mu or not
    for (i in 1:reps) {
      lines(c(lci[i],uci[i]),rep(i,2),col=colr[i])                                            # Put CIs on the plot
      points(rnd.mns[i],i,pch=19,col=colr[i])                                                 # Put means on the plot
    }
    hit <- length(colr[colr=="black"])                                                        # How many CIs contained mu
    mtext(paste(hit," of ",reps," (",formatC(100*hit/reps,format="f",digits=1),"%) captured the popn mean",sep=""),line=2,cex=1.25,col="red")
    if (tail=="less.than") { avg.me <- mean(uci-rnd.mns) }                                         # Calculate average margin-of-error
    else { avg.me <- mean(rnd.mns-lci)}
    mtext(paste("Average margin-of-error is ",round(avg.me,1),sep=""),line=0.5,cex=1.25,col="red")
    invisible(NULL)
  } # end ciSimPlot internal function
  
  # begin main function
  if (!require(relax)) warning("This function requires that you have the relax package installed.",call.=FALSE)
  else {
    method <- match.arg(method)
    relax::gslider(refresh,prompt=TRUE,
               sl.names=   c( "n", "Confidence (C)", "Tail Type (-1=less,1=grtr)"),
               sl.mins=    c(  10,             0.80,                -1),
               sl.maxs=    c( 100,             0.99,                 1),
               sl.deltas=  c(   5,             0.01,                 1),
               sl.defaults=c(  10,             0.95,                 0),
               title = "Confidence Region Simulator",
               but.functions= function(...){relax::slider(obj.name="rerand",obj.value="Y");refresh()}, but.names=c("Re-Randomize"),
               pos.of.panel="left",vscale=1.5)
  }
}


