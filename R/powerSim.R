#'Dynamic simulation of power calculation in 1-sample mean problems.
#'
#'This function plots the null and actual distributions, highlights the
#'rejection region and the power region, and allows the user to manipulate the
#'effect size, standard deviation, sample size, and alpha to determine the
#'effect on power.
#'
#'@param mu0 A single numeric that is the null hypothesized mean.
#'@param s.mua A single numeric that is the starting value for the actual population mean.
#'@param s.sigma A single numeric that is the starting value for the actual
#'population standard deviation.
#'@param s.n A single numeric that is the starting value for the sample size.
#'@param s.alpha A single numeric that is the starting value for alpha.
#'@param lower.tail A single logical indicating if the rejection region is into
#'the lower tail or not.
#'@return None, but a dynamic graphic with slider bars is produced.
#'@keywords dynamic
#'@examples
#'if (interactive()) {
#'
#'powerSim()
#'powerSim(lower.tail=FALSE)
#'
#'}
#'@export
#'
powerSim <- function(mu0=100,s.mua=95,s.sigma=10,s.n=30,s.alpha=0.05,lower.tail=TRUE) {
  refresh <- function(...) {
    mua <- relax::slider(no=1)
    sigma <- relax::slider(no=2)
    n <- relax::slider(no=3)
    alpha <- relax::slider(no=4)
    powerSimPlot(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail)
  }
  
  powerSimPlot <- function(mua,sigma,n,alpha,mu0,s.mua,s.sigma,s.n,lower.tail){
    old.par <- par(mgp=c(2,0.75,0),mfrow=c(2,1)); on.exit(par(old.par))
    SE <- sigma/sqrt(n)
    xlmts <- c(min(mu0,s.mua)-1*s.sigma,max(mu0,s.mua)+1*s.sigma)
    ylmts <- c(0,dnorm(0,0,s.sigma/sqrt(2*s.n)))
    ifelse(lower.tail, cv <- qnorm(alpha,mu0,SE), cv <- qnorm(1-alpha,mu0,SE))   # Find critical value
    cv <- round(cv,2) 
    x0 <- sort(c(cv,seq(-4*SE+mu0,4*SE+mu0,by=0.01)))                            # Create the null distribution values
    norm0 <- dnorm(x0,mu0,SE)
    xa <- sort(c(cv,seq(-4*SE+mua,4*SE+mua,by=0.01)))                            # create the actual distribution values
    norma <- dnorm(xa,mua,SE)
    par(mar=c(2,1,2,3))
    c.region(cv,x0,norm0,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,shade.col="red",lbl.col="red",show.lbl=TRUE,xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
    mtext("Null Dist.",4,cex=1.5,line=1)
    lines(c(mu0,mu0),c(0,max(norm0)),lty=3,lwd=2,col="blue")
    abline(v=cv,lty=2,lwd=2,col="red")
    ifelse(lower.tail,pos <- 2, pos <- 4)
    text(cv,0.9*ylmts[2],"Reject Ho",pos=pos,col="red")
    mtext(paste("mu=",mua,", sigma=",sigma,", n=",n,", alpha=",format(alpha,nsmall=2)),3,line=0.5)
    
    par(mar=c(3,1,1,3))
    c.region(cv,xa,norma,lower.tail,area=NULL,plot=TRUE,show.ans=FALSE,shade.col="green",lbl.col="green",show.lbl=FALSE,xlab="",xlim=xlmts,ylim=ylmts,yaxt="n",ylab="")
    mtext("means",1,line=1.5,cex=1.25)
    mtext("Actual Dist.",4,cex=1.5,line=1)
    lines(c(mua,mua),c(0,max(norma)),lty=3,lwd=2,col="blue")
    abline(v=cv,lty=2,lwd=2,col="red")
    abline(v=mu0,lty=3,lwd=2,col="gray")
    
    pwr <- round(pnorm(cv,mua,SE),3)
    ifelse(lower.tail, pos <- "topleft", pos <- "topright")
    legend(pos,legend=paste("power=",format(pwr,nsmall=3)),bty="n")
    ifelse(lower.tail, pos <- "topright", pos <- "topleft")
    legend(pos,legend=paste("beta=",format(1-pwr,nsmall=3)),bty="n")    
  } # end powerSimPlot internal function

  # Start of main function
  if (!requireNamespace("relax")) warning("This function requires that you have the relax package installed.",call.=FALSE)
  else {
    relax::gslider(refresh,prompt=TRUE,vscale=1.5,
             sl.names=   c(             "Actual mu",   "sigma", "n", "alpha"),
             sl.mins=    c(min(mu0,s.mua)-2*s.sigma,         1,   2,    0.01),
             sl.maxs=    c(max(mu0,s.mua)+2*s.sigma, 3*s.sigma, 100,    0.30),
             sl.deltas=  c(                       1,         1,   1,    0.01),
             sl.defaults=c(                   s.mua,   s.sigma, s.n, s.alpha),
             title = "Power Simulator",pos.of.panel="left")
  }
}
