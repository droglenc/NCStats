#'Constructs a plot of depth versus limnological measure.
#'
#'Constructs a plot of depth versus limnological measure with the depth
#'increasing from top to bottom.
#'
#'@param depth A vector of depths at which the limnological measure was recorded.
#'@param measure A vector of the recorded limnological measure.
#'@param type A string indicating the type of plot to construct.  Use \code{'p'}
#'for points, \code{'l'} for lines, or \code{'b'} (default) for both.
#'@param \dots Other arguments to be passed to the \code{plot} function.
#'@return None.  However, a plot is produced.
#'@export
#'@keywords hplot
#'@examples
#'## Winter temperature profile -- Happles Lake, WI
#'depth <- 1:32
#'temp <- c(1,rep(4.2,24),4.6,4.6,4.8,5,5,5,5)
#'limnoProfilePlot(depth,temp,xlab="Temperature (C)",ylab="Depth (ft)")
#'# add marker for ice depth (20")
#'abline(h=-20/12,lty=3)
#'axis(2,-20/12,"Ice",las=1)
#'
#'## Side-by-side winter temp and DO profile -- Happles Lake, WI
#'DO <- c(7.85,6.8,6.8,6.8,6.8,6.85,6.8,6.8,6.85,6.8,6.8,6.8,6.8,6.8,6.8,6.8,6.7,
#'        6.7,6.7,6.6,6.6,6.6,6.6,6.3,6.0,2.6,2.1,1.2,1.1,0.6,0.6,0.45)
#'op <- par(mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0),mfrow=c(1,2))
#'limnoProfilePlot(depth,temp,xlab="Temperature (C)",ylab="Depth (ft)")
#'abline(h=-20/12,lty=3)
#'axis(2,-20/12,"Ice",las=1)
#'limnoProfilePlot(depth,DO,xlab="Dissolved Oxygen (mg/l)",ylab="Depth (ft)",xlim=c(0,8))
#'abline(h=-20/12,lty=3)
#'axis(2,-20/12,"Ice",las=1)
#'par(op)
#'
limnoProfilePlot <- function(depth,measure,type="b",...) {
  ndepth <- -depth
  plot(ndepth~measure,type=type,yaxt="n",...)
  yaxs.vals <- axTicks(side=2)
  axis(2,at=yaxs.vals,labels=-yaxs.vals)
}
