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
  if (iChk4Namespace("FSA")) {
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