#'Dynamic simulations to demonstrate `how' the mean and median are computed.
#'
#'This function visually illustrates how the mean \dQuote{balances} distances
#'and the median \dQuote{balances} individuals in a random sample from a known
#'population.  The user can manipulate the number of individuals in the sample
#'and the shape of the population to determine how these attributes affect the
#'calculation of the mean and median.
#'
#'@param x An optional numeric vector (of actual data) to be used in the visual.
#'@param outlier A string indicating whether the outlier should be modeled at the
#'maximum (\code{="max"}), minimum (\code{="min"}), or not at all \code{="none"}).
#'This is ignored if \code{x} is not \code{NULL}.
#'@return None, but a graphic (if \code{x} is not \code{null}) or a dynamic
#'graphic with slider bars (if \code{x} is \code{null}) is produced.
#'@keywords dynamic
#'@examples
#'if (interactive()) {
#'
#'## Examples with simulated data
#'##   no outlier
#'meanMedian()
#'
#'##   outlier at minimum
#'meanMedian(outlier="min")
#'
#'##   no outlier
#'meanMedian(outlier="max")
#'
#'}
#'
#'## Example with user-derived data
#'meanMedian(c(1:7,25))
#'
#'@export
meanMedian <- function(x=NULL,outlier=c("none","max","min")) {
  if (!is.null(x)) {
    if (!is.vector(x) | !is.numeric(x)) stop("'x' must be a numeric vector.",call.=FALSE)
    meanMedianPlots(x)
  } else {
    outlier <- match.arg(outlier)
    if (!require(relax)) warning("This function requires that you have the relax package installed.",call.=FALSE)
    else meanMedianBeta(outlier)
  }
}

meanMedianPlots <- function(x) {
  x <- x[order(x)]
  mn.x <- mean(x)
  mdn.x <- median(x)
  n <- length(x)
  mdn.pos <- (n+1)/2
  old.par <- par(mfcol=c(2,1)); on.exit(par(old.par))
  # Histogram with the mean and median shown by vertical lines
  par(mar=c(0.05,3.5,2,1),mgp=c(2,0.5,0),tcl=-0.2,xaxt="n")
  hist(x,main="",col="gray90",right=FALSE)
  abline(h=0)
  abline(v=mn.x,col="red",lwd=2)
  abline(v=mdn.x,col="blue",lwd=2)
  mtext(paste("Median =",formatC(mdn.x,format="f",digits=2),ifelse(mdn.x>mn.x," *","")),line=1,col="blue")
  mtext(paste("Mean =",formatC(mn.x,format="f",digits=2),ifelse(mn.x>mdn.x," *","")),line=0,col="red")
  # Scatterlot of values against individual number for illustrating the median
  par(mar=c(3.5,3.5,0.05,1),mgp=c(2,0.5,0),tcl=-0.2,xaxt="s")
  plot(x,1:n,xlab="Quantitative Variable",ylab="Ordered Individual Number",col.main="blue",xlim=range(pretty(range(x),n=nclass.Sturges(x)))) 
  abline(v=mdn.x,col="blue",lwd=2)                # Put median on the plot
  if (mdn.pos == floor(mdn.pos)) { 
    lwr <- 1:(mdn.pos-1)
    upr <- (mdn.pos+1):n
  } else { 
    lwr <- 1:floor(mdn.pos)
    upr <- (floor(mdn.pos)+1):n
  }
  points(x[lwr],lwr,col="black",pch=19)           # color points relative to the median
  points(x[upr],upr,col="red",pch=19)  
  abline(v=mn.x,col="red",lwd=2)                  # Put mean on the plot
  for (i in 1:n) {                                # put residuals from mean on graph
    if (x[i] < mn.x) lines(c(x[i],mn.x),c(i,i),col="black",lty=3)
    else lines(c(x[i],mn.x),c(i,i),col="red",lty=3)
  } } # end internal meanMedianPlots

meanMedianBeta <- function(outlier) {
  refresh <- function(...) {
    x <- rbeta(relax::slider(no=1), relax::slider(no=2), relax::slider(no=3)) # get random data
    x <- x-min(x)           # rescale to a min of 0
    x <- x/max(x)           # and a max of 1
    if (outlier=="max") {
      x <- x/2              # rescale max to 0.5
      x[length(x)] <- 1     # but replace max with 1 to be an outlier
    } else if (outlier=="min") {
      x <- (x+1)/2          # rescale to min of 0.5 and max of 1
      x[1] <- 0             # but replace min with 0 to be an outlier
    }
    meanMedianPlots(x)
  } # end refresh
  
  relax::gslider(refresh,prompt=TRUE,vscale=1.5,hscale=1.75,
                 sl.names=   c("Individuals", "Shape 1 (alpha)", "Shape 2 (beta)"),
                 sl.mins=    c(           10,                 1,                1),
                 sl.maxs=    c(          100,                10,               10),
                 sl.deltas=  c(            1,                 1,                1),
                 sl.defaults=c(           30,                 1,                1),
                 title = "Mean vs. Median Simulator",
                 but.functions= function(...){relax::slider(obj.name="rerand",obj.value="Y");refresh()}, but.names=c("Re-Randomize"),
                 pos.of.panel="left")
}
