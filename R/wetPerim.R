#'Computes the wetted perimeter of a stream from depth and distance measurements.
#'
#'Computes the wetted perimeter of a stream from depth and distance measurements.
#'
#'@aliases wetPerim summary.wetPerim plot.wetPerim
#'@param distance A vector of distance measurements at intervals across the
#'cross section.
#'@param depth A vector of depth measurements at intervals across the cross-section.
#'@param object An object returned from \code{discharge}.
#'@param x An object returned from \code{discharge}.
#'@param detail A logical indicating whether the details of the discharge
#'calculations should be shown (\code{=TRUE}) or not.
#'@param pch The plotting character to be used for the depth profile.  See \code{par}.
#'@param xlab A string for labeling the x-axis.
#'@param ylab A string for labeling the y-axis.
#'@param newwin A logical indicating whether a new graphics window should be
#'opened (only for Windows operating systems).
#'@param \dots Other arguments to be passed to the \code{plot} function.
#'@return \code{wetPerim} produces a list with a data frame of the original
#'data, a vector of individual perimeter values, and the total perimeter.
#'\code{plot} method produce a plot demonstrating the calculations.
#'\code{summary} returns the total wetted perimeter and detailed intermediate
#'calculations if asked for.
#'@export
#'@keywords hplot models
#'@examples
#'dist1 <- c(seq(0,22,2),24.7)
#'dep1 <- c(0,.9,1.1,1.1,1.1,.9,.9,.9,.9,.9,.9,.7,0)
#'vel1 <- c(0,1.01,1.07,1.22,1.22,1.36,1.28,1.38,1.29,0.88,0.52,0,0)
#'ex1w <- wetPerim(dist1,dep1)
#'summary(ex1w)
#'plot(ex1w)
#'
#'dist2 <- c(seq(0,18,2),19.7)
#'dep2 <- c(0,1.2,1.5,1.4,1.4,1.4,1.5,1.7,2,1.5,0)
#'vel2 <- c(0,.53,.76,.95,.90,.98,1.01,.9,.9,0,.0)
#'ex2w <- wetPerim(dist2,dep2)
#'summary(ex2w)
#'plot(ex2w)
#'
#'@rdname wetPerim
#'@export wetPerim
wetPerim <- function(distance,depth) {
  raw.df <- data.frame(b=distance,d=depth)
  numpts <- nrow(raw.df)
  wprm <- sqrt(diff(distance)^2+diff(depth)^2)
  res <- list(raw.df=raw.df,perims=wprm,wetperim=sum(wprm))
  class(res) <- "wetPerim"
  invisible(res)
}

#'@rdname wetPerim
#'@method summary wetPerim
#'@S3method summary wetPerim
summary.wetPerim <- function(object,detail=TRUE,...) {
  if (detail) {
    print(object$perims)
    cat("\n\n")
  }
  cat(paste("Total wetted perimeter is ",formatC(object$wetperim,format="f",digits=3),".\n\n",sep=""))
}

#'@rdname wetPerim
#'@method plot wetPerim
#'@S3method plot wetPerim
plot.wetPerim <- function(x,pch=19,xlab="Distance (ft)",ylab="Depth (ft)",newwin=TRUE,...) {
 # make the baseplot
  if(.Platform$OS.type == "windows" & newwin) windows(7,3.5)
  old.par <- par(mar=c(3.5,3.5,3,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
  numpts <- nrow(x$raw.df)
  ndepth <- -x$raw.df$d                                                          # needed so the depths increase as you move down the y-axis
  dstnc <- x$raw.df$b
  plot(ndepth~dstnc,type="n",xlab=xlab,ylab=ylab,yaxt="n",xaxt="n",bty="n",...)  # base plot
  yaxs.vals <- axTicks(side=2)                                                   # label axes -- x will be labeled with interval distances
  axis(2,at=yaxs.vals,labels=-yaxs.vals)
  axis(1,at=dstnc,labels=dstnc)
 # put on the ground
  polygon(c(-5,dstnc,1.5*dstnc[numpts],1.5*dstnc[numpts],-5,-5),c(ndepth[1],ndepth,ndepth[numpts],2*min(yaxs.vals),2*min(yaxs.vals),ndepth[1]),col="wheat4") 
 # find y location for labels
  yloclab <- 0.10*diff(range(yaxs.vals))
 # put on stream cross-section
  abline(h=0,lwd=3)                                                              # stream water surface
  lines(ndepth~x$raw.df$b,lwd=3)
  points(ndepth~x$raw.df$b,pch=pch)
 # place perimeter values across the top \
  w <- diff(x$raw.df$b)
  for (i in 1:(numpts-1)) {
    text(x$raw.df$b[i]+w[i]/2,yloclab,formatC(x$perims[i],digits=2,format="f"),xpd=TRUE)            # place discharges across the top
  }    

 # put total perimeter in title heading
  text(x$raw.df$b[numpts]/2,2.5*yloclab,paste("Total Wetted Perimeter =",formatC(x$wetperim,digits=2,format="f")),xpd=TRUE,cex=1.25)  
}
