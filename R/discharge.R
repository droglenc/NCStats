#'Conmputes stream discharge measurements using the mean-section or mid-section methods.
#'
#'Computes a cross-section profile of a stream demonstrating distance, velocity, and discharge measurements for the mean-section and mid-section methods.
#'
#'@aliases discharge summary.discharge plot.discharge
#'
#'@param distance A vector of distance measurements at intervals across the cross section.
#'@param depth A vector of depth measurements at intervals across the cross section.
#'@param velocity A vector of velocity measurements at intervals across the cross section.
#'@param type A string indicating the discharge method to use.
#'@param object An object returned from \code{discharge}.
#'@param x An object returned from \code{discharge}.
#'@param detail A logical indicating whether the details of the discharge calculations should be shown (\code{=TRUE}) or not.
#'@param pch The plotting character to be used for the depth profile.  See \code{par}.
#'@param xlab A string for labeling the x-axis.
#'@param ylab A string for labeling the y-axis.
#'@param newwin A logical indicating whether a new graphics window should be opened (only for Windows operating systems).
#'@param \dots Other arguments to be passed to the \code{plot} function.
#'
#'@return \code{discharge} produces a list with a data frame of the original
#'data, a data frame of computed values, the \code{type=} argument, and a label
#'for the type of argument.  \code{plot} method produce a plot demonstrating
#'the calculations.  \code{summary} returns the total discharge calculation and
#'detailed intermediate calculations if asked for.
#'
#'@keywords hplot models
#'
#'@examples
#'
#'dist1 <- c(seq(0,22,2),24.7)
#'dep1 <- c(0,.9,1.1,1.1,1.1,.9,.9,.9,.9,.9,.9,.7,0)
#'vel1 <- c(0,1.01,1.07,1.22,1.22,1.36,1.28,1.38,1.29,0.88,0.52,0,0)
#'ex1 <- discharge(dist1,dep1,vel1)
#'summary(ex1)
#'plot(ex1)
#'ex1a <- discharge(dist1,dep1,vel1,type="Mid")
#'summary(ex1a)
#'plot(ex1a)
#'
#'dist2 <- c(seq(0,18,2),19.7)
#'dep2 <- c(0,1.2,1.5,1.4,1.4,1.4,1.5,1.7,2,1.5,0)
#'vel2 <- c(0,.53,.76,.95,.90,.98,1.01,.9,.9,0,.0)
#'ex2 <- discharge(dist2,dep2,vel2)
#'summary(ex2)
#'plot(ex2)
#'ex2a <- discharge(dist2,dep2,vel2,type="Mid")
#'summary(ex2a)
#'plot(ex2a)
#'
#'@rdname discharge
#'@export
discharge <- function(distance,depth,velocity,type=c("Mean","Mid")) {
  discharge.mean.section <- function(raw.df,numpts) { # mean section method
    w <- diff(raw.df$b)                                                         # compute interval widths
    dbar <- raw.df$d[-numpts]+diff(raw.df$d)/2                                  # compute average depth between adjacent intervals
    vbar <- raw.df$v[-numpts]+diff(raw.df$v)/2                                  # compute average velocity between adjacent intervals
    q <- w*dbar*vbar                                                            # find rectangle discharge
    calc.df <- data.frame(vbar=vbar,w=w,dbar=dbar,q=q)
  }
    
  discharge.mid.section <- function(raw.df) { # mid section method
    w <- diff(raw.df$b)                                                         # compute interval widths
    w.n1 <- w[-length(w)]
    w.p1 <- w[-1]
    wids <- c(0,(w.n1+w.p1)/2,0)                                                # find half-widths on either side of midpoint
    q <- raw.df$v*raw.df$d*wids                                                 # find rectangle discharge
    calc.df <- data.frame(v=raw.df$v,wids=wids,d=raw.df$d,q=q)
  }

  type <- match.arg(type)
  raw.df <- data.frame(b=distance,d=depth,v=velocity)
  numpts <- nrow(raw.df)
  if (type=="Mean") {
    calc.df <- discharge.mean.section(raw.df,numpts)
    type.lbl <- "Mean-Section Method"
  } else {
    calc.df <- discharge.mid.section(raw.df)
    type.lbl <- "Mid-Section Method"
  }
  q <- sum(calc.df$q)
  res <- list(raw.df=raw.df,calc.df=calc.df,discharge=q,type=type,type.lbl=type.lbl)
  class(res) <- "discharge"
  invisible(res)
}

#'@rdname discharge
#'@export
summary.discharge <- function(object,detail=TRUE,...) {
  if (detail) {
    print(object$calc.df)
    cat("\n\n")
  }
  cat("Total discharge, computed with the ",object$type.lbl,", is ",object$discharge,".\n\n",sep="")
}

#'@rdname discharge
#'@export
plot.discharge <- function(x,pch=19,xlab="Distance (ft)",ylab="Depth (ft)",newwin=TRUE,...) {
  mean.section.rects <- function(x,numpts,yloclab) { # put on rectangles, velocities in rectangle, discharges on top for mean section method
    vel.col <- color.scale(x$calc.df$vbar,0,c(1,0.5),c(1,0.5))
    for (i in 1:(numpts-1)) {
      rect(x$raw.df$b[i],-x$calc.df$dbar[i],x$raw.df$b[i+1],0,col=vel.col[i])                                      # make rectangles
      text(x$raw.df$b[i]+x$calc.df$w[i]/2,-x$calc.df$dbar[i]*0.6,formatC(x$calc.df$vbar[i],digits=2,format="f"))   # place velocities at 60% mark
      text(x$raw.df$b[i]+x$calc.df$w[i]/2,yloclab,formatC(x$calc.df$q[i],digits=2,format="f"),xpd=TRUE)            # place discharges across the top
    }    
  }

  mid.section.rects <- function(x,numpts,yloclab) { # put on rectangles, velocities in rectangle, discharges on top for mid section method
    vel.col <- color.scale(x$raw.df$v,0,c(1,0.5),c(1,0.5))
    for (i in 1:(numpts-1)) {
      rect(x$raw.df$b[i]-x$calc.df$wids[i]/2,-x$raw.df$d[i],x$raw.df$b[i]+x$calc.df$wids[i]/2,0,col=vel.col[i])    # make rectangles
      if (i>1) text(x$raw.df$b[i],-x$raw.df$d[i]*0.6,formatC(x$raw.df$v[i],digits=2,format="f"))                   # place velocities at 60% mark
      text(x$raw.df$b[i],yloclab,formatC(x$calc.df$q[i],digits=2,format="f"),xpd=TRUE)                             # place discharges across the top
    }
  }

 # make the baseplot
  numpts <- nrow(x$raw.df)
  ndepth <- -x$raw.df$d                                                               # needed so the depths increase as you move down the y-axis
  dstnc <- x$raw.df$b
  plot(ndepth~dstnc,type="n",xlab=xlab,ylab=ylab,yaxt="n",xaxt="n",bty="n",...)  # base plot
  yaxs.vals <- axTicks(side=2)                                                        # label axes -- x will be labeled with interval distances
  axis(2,at=yaxs.vals,labels=-yaxs.vals)
  axis(1,at=dstnc,labels=dstnc)
 # put on the ground
  polygon(c(-5,dstnc,1.5*dstnc[numpts],1.5*dstnc[numpts],-5,-5),c(ndepth[1],ndepth,ndepth[numpts],2*min(yaxs.vals),2*min(yaxs.vals),ndepth[1]),col="wheat4") 
 # find y location for labels
  yloclab <- 0.10*diff(range(yaxs.vals))
 # plot the rectangles
  if (x$type=="Mean") { mean.section.rects(x,numpts,yloclab) }
    else mid.section.rects(x,numpts,yloclab)
 # put on stream cross-section
  abline(h=0,lwd=3)                                                              # stream water surface
  lines(ndepth~x$raw.df$b,lwd=3)
  points(ndepth~x$raw.df$b,pch=pch)
 # put total dischage in title heading
  text(x$raw.df$b[numpts]/2,2.5*yloclab,paste(x$type.lbl,"Total Discharge =",formatC(x$discharge,digits=2,format="f")),xpd=TRUE,cex=1.25)  
}
