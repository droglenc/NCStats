#' Internal functions used in NCStats.
#' 
#' Internal functions used in NCStats
#' 
#' These functions perform code required by other functions in the NCStats
#' package.  These functions are not to be called by the user.
#' 
#' @note Take note of the following uses:
#'   \itemize{
#'     \item \code{c.region} and \code{d.region} are used in \code{\link{distrib}}, \code{\link{plot.htest}} and \code{\link{powerSim}}.
#'     \item \code{cCDF.plot}, \code{cPDF.plot}, \code{dCDF.plot}, and \code{dPDF.plot} are all used in the distribution simulators -- e.g., \code{\link{sbeta}}, \code{\link{snorm}}, etc..
#'     \item \code{assumPlot_ANOVA}, \code{assumPlot_REGRESS}, \code{lblADTest}, \code{lblNCVTest}, \code{lblOutTest}, and \code{lblLevTest} functions are all used in \code{\link{transChooser}}.
#' }
#' 
#' @rdname NCStats-internals
#' @name NCStats-internals
#' 
#' @keywords internal
#' 
#' @aliases .onAttach cCDF.plot cPDF.plot dCDF.plot dPDF.plot c.region d.region assumPlot_ANOVA assumPlot_REGRESS lblADTest lblNCVTest lblOutTest lblLevTest
#' 

##################################################################
## Sends a start-up message to the console when the package is loaded.
##################################################################
.onAttach <- function(lib,pkg,...) {
  ## Get version number -- basically code from readPkgVersion in SweaveListingUtils
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),fields="Version")
  ## Send message
  msg <- paste("\n\n")
  msg <- paste(msg,"############################################\n")
  msg <- paste(msg,"##","NCStats package, version",vers,"        ##\n")
  msg <- paste(msg,"##   by Derek H. Ogle, Northland College  ##\n")
  msg <- paste(msg,"##                                        ##\n")
  msg <- paste(msg,"##    type ?NCStats for documentation.    ##\n")
  msg <- paste(msg,"############################################\n\n")
  packageStartupMessage(msg)
}


##################################################################
# Check if a required namespaced can be loaded and, if not, send
#   an error message.
##################################################################
iChk4Namespace <- function(pkg) {
  res <- requireNamespace(pkg,quietly=TRUE)
  if (!res) stop(paste0("The '",pkg," package must be installed."))
  res
}


##################################################################
# Internal functions used in distrib().
##################################################################
c.region <- function(xval,x,fx,lower.tail,area,plot,show.ans,shade.col,lbl.col,show.lbl,cex.ans=1,...) {
  if (lower.tail) {
    x.shade <- x[x<=xval];                  y.shade <- fx[x<=xval]
    x.shade <- c(x.shade,xval,xval,min(x)); y.shade <- c(y.shade,fx[x==xval],0,0)
  } else {
    x.shade <- x[x>=xval];                  y.shade <- fx[x>=xval]
    x.shade <- c(xval,x.shade,max(x),xval); y.shade <- c(fx[x==xval],y.shade,0,0)  
  }
  if (plot) {
    cPDF.plot(x,fx,show.mnsd=FALSE,...)
    if (show.lbl) axis(1,at=xval,labels=round(xval,3),line=0.9,tcl=1.5,fg=lbl.col,col.axis=lbl.col)
    polygon(x.shade,y.shade,col=shade.col,border=shade.col)
    lines(x,fx)
    if(show.ans) mtext(paste("Value =",round(xval,3),"; Area =",round(area,4)),line=0.2,col="red",cex=cex.ans)
  }
  invisible(list(x=x,fx=fx,x.shade=x.shade,y.shade=y.shade))
}

cCDF.plot <- function(x,Fx,...) {
  plot(x,Fx,type="l",lwd=2,ylim=c(0,1),...)
}

cPDF.plot <- function(x,fx,show.mnsd=TRUE,...) {
  plot(x,fx,type="l",lwd=2,...)
  if (show.mnsd) {
    x.vals <- rep(x,round(10000*fx,0)); mu <- mean(x.vals); sigma <- sd(x.vals)
    mtext(paste("Mean = ",round(mu,1),", SD = ",round(sigma,1)),line=0.25,col="blue")
    d.max <- max(fx)
    lines(rep(mu,2),c(0,d.max),col="blue",lwd=2)
    lines(c(mu-sigma,mu+sigma),rep(0.6*d.max,2),col="blue",lwd=2)
  }
}

d.region <- function(xval,x,fx,region,area,reverse,plot,show.ans,shade.col,lbl.col,cex.ans=1,...) {
  bp <- dPDF.plot(x,fx,show.mnsd=FALSE,...)
  switch(region,
         lower={
           xleft <- bp[x<=xval]-0.5; xright <- xleft+1
           ytop <- fx[x<=xval];      ybottom <- 0    
         },
         upper={
           ifelse(reverse,adjval <- xval-1, adjval <- xval)
           xleft <- bp[x>adjval]-0.5; xright <- xleft+1
           ytop <- fx[x>adjval];      ybottom <- 0
         },
         single={
           xleft <- bp[x==xval]-0.5; xright <- xleft+1
           ytop <- fx[x==xval];      ybottom <- 0
         } )
  if (plot) {
    rect(xleft,ybottom,xright,ytop,col=shade.col)
    if(show.ans) mtext(paste("Value =",xval,"; Probability =",round(area,4)),line=0.2,col="red",cex=cex.ans)  
  }
  invisible(list(xleft,xright,ybottom,ytop))
}

dCDF.plot <- function(x,Fx,...) {
  plot(x,Fx,pch=19,ylim=c(0,1),...)
  for (i in 1:(length(x)-1)) {
    lines(c(x[i+1],x[i+1]),c(Fx[i],Fx[i+1]),lwd=2,col="gray")
    lines(c(x[i],x[i+1]),c(Fx[i],Fx[i]),lwd=2)
  }
  points(x,Fx,pch=19)
}

dPDF.plot <- function(x,fx,show.mnsd=TRUE,...) {
  bp <- barplot(fx,space=0,col="white",...)
  axis(1,at=bp,labels=x)
  if (show.mnsd) {
    mu <- sum(x*fx); sigma <- sqrt(sum(((x-mu)^2)*fx))
    mtext(paste("Mean = ",round(mu,1),", SD = ",round(sigma,1)),line=0.25,col="blue")
    mu.pos <- mu+bp[1]  # needed to place values on plot properly
    lines(rep(mu.pos,2),c(0,max(fx)),col="blue",lwd=2)
    lines(c(mu.pos-sigma,mu.pos+sigma),rep(0.6*max(fx),2),col="blue",lwd=2)
  }
  invisible(bp)
}


