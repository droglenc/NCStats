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
#' if (interactive()) {
#' cltSim()
#' }
#' 
#' @export
cltSim <- function(reps=1000,incl.norm=FALSE) {
  iCLTrefresh <- function(...) {
    n <- relax::slider(no=1)
    sh1 <- relax::slider(no=2)
    sh2 <- relax::slider(no=3)
    iCLTSimPlot(n,sh1,sh2,reps,incl.norm)
  } # end iCLTrefresh internal function
  
  # begin main function
  if (!requireNamespace("relax")) warning("This function requires that you have the relax package installed.",call.=FALSE)
  else {
    relax::gslider(iCLTrefresh,prompt=TRUE,hscale=2,
           sl.names=   c( "n", "Shape 1 (alpha)", "Shape 2 (beta)"),
           sl.mins=    c(  10,                 1,                1),
           sl.maxs=    c( 100,                20,               20),
           sl.deltas=  c(   5,                 1,                1),
           sl.defaults=c(  10,                 1,                1),
           title = "Sampling Distribution Simulator",
           but.functions= function(...){
             relax::slider(obj.name="rerand",obj.value="Y");iCLTrefresh()
           },
           but.names=c("Re-Randomize"),
           pos.of.panel="left")
  }
}

iCLTSimPlot <- function(n,sh1,sh2,reps,incl.norm) {
  # create values for popn graph  
  x <- seq(0,0.95,0.05)
  y <- round(1000*stats::dbeta(x,sh1,sh2),0)
  x <- rep(x,y)
  # Plot the population distribution
  old.par <- graphics::par(mar=c(3,2,2.25,1),mgp=c(2,0.4,0),tcl=-0.2,mfcol=c(1,2))
  on.exit(graphics::par(old.par))
  graphics::hist(x,right=FALSE,freq=FALSE,breaks=seq(0,1,0.05),yaxt="n",
                 xlab="Variable (X)",ylab="",main="Population Distribution",col="gray90")
  graphics::mtext("Frequency of Individuals",2,line=0.5)
  # Show mean and SD of population on population distribution
  mn.pop <- sh1/(sh1+sh2)
  sd.pop <- sqrt((sh1*sh2)/(((sh1+sh2)^2)*(sh1+sh2+1)))
  graphics::mtext(paste("Mean =",round(mn.pop,3),"SD =",round(sd.pop,3)),line=-0.4,col="red")
  # Construct random samples from population
  rnd.reps <- matrix(stats::rbeta(n*reps,sh1,sh2),nrow=n)
  # compute means of random samples
  rnd.mns <- apply(rnd.reps,2,mean)
  # Plot the sampling distribution
  # Set limits differently depending on sh1 and sha2
  if (!(sh1+sh2)==2) { lmts <- stats::quantile(x,c(0.10,0.90)) } 
  else { lmts <- stats::quantile(x,c(0.05,0.95)) }
  mns.hist <- graphics::hist(rnd.mns,freq=FALSE,breaks=15,xlim=lmts,yaxt="n",
                   xlab=expression(paste("Mean( ",bar(X)," )")),
                   ylab="",main="Sampling Distribution",col="gray90")
  graphics::mtext("Frequency of Samples",2,line=0.5)
  # compute mean and SE of sampling dist & shown on the sampling distribution
  mn.mns <- mean(rnd.mns)
  sd.mns <- stats::sd(rnd.mns)                                              
  graphics::mtext(paste("Mean =",round(mn.mns,3),"SE =",round(sd.mns,3)),line=-0.4,col="red")
  # vertical line at mean
  graphics::lines(c(mn.mns,mn.mns),c(0,max(mns.hist$density)),col="red",lwd=2)
  # horiz line represent +/- SD of mean
  graphics::lines(c(mn.mns-sd.mns,mn.mns+sd.mns),rep(0.6*max(mns.hist$density),2),col="red",lwd=2)
  if (incl.norm) {
    norm.vals <- seq(min(mns.hist$breaks),max(mns.hist$breaks),length.out=50)
    graphics::lines(norm.vals,dnorm(norm.vals,mn.mns,sd.mns),col="blue")
  }
} # end iCLTSimPlot internal function
