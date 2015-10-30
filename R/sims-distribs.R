#' @title Demonstrates the shape of the PDF and CDF for the given distribution.
#' 
#' @description Dynamically demonstrates the effect of parameter choices on the shape of the PDF and CDF for the given distribution.
#' 
#' @details If the user is using RStudio and the \pkg{manipulate} package is installed then a dynamic graph is produced in the \dQuote{Plots} pane of RStudio.  The plot controls may be accessed through the \dQuote{gear} that is in the upper-left corner of the plot.  The sliders may be used to change the parameters of the distribution.  The real-time updating of the graphic allows the user to determine the effect of changing each parameter on the shape, mean, and standard deviation of the distribution.  
#' 
#' @aliases sbeta sbinom schisq sexp sf sgamma sgeom shyper slnorm snbinom snorm spois st
#' 
#' @return None, but a dynamic graphic with slider bars will be produced.
#' 
#' @seealso \code{\link[TeachingDemos]{vis.binom}}, \code{\link[TeachingDemos]{vis.gamma}}, \code{\link[TeachingDemos]{vis.normal}}, and \code{\link[TeachingDemos]{vis.t}} of \code{TeachingDemos}.  Also see \code{\link[stats]{dbeta}}, \code{\link[stats]{dbinom}}, \code{\link{dchisq}}, etc.
#' 
#' @keywords hplot distribution dynamic
#' 
#' @examples
#' \dontrun{
#' sbeta()
#' sbinom()
#' schisq()
#' sexp()
#' sf()
#' sgamma()
#' sgeom()
#' shyper()
#' slnorm()
#' snbinom()
#' snorm()
#' spois()
#' st()
#' }
#' 
#' @rdname simDistrib
#' @export
sbeta <- function() { # Beta Distribution Simulator
  show.mnsd <- show.both <- alpha <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(0,1)  #range for plotting
        x <- seq(0.01,0.99,by=0.01)
        fx <- stats::dbeta(x,alpha,beta)
        Fx <- stats::pbeta(x,alpha,beta)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="X",ylab="F(x)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
        }
      }, alpha=manipulate::slider(0.2,5,step=0.1,initial=1),
         beta=manipulate::slider(0.2,5,step=0.1,initial=1),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
schisq <- function() { # chi-square Distribution Simulator
  show.mnsd <- show.both <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(0,50)
        x <- seq(0.01,xr[2],by=0.01)
        fx <- stats::dchisq(x,df)
        Fx <- stats::pchisq(x,df)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="Chi-Square",ylab="f(chi)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="Chi-Square",ylab="F(chi)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="Chi-Square",ylab="f(chi)",main="PDF",xlim=xr,show.mnsd)
        }
      }, df=manipulate::slider(1,20,step=1,initial=4),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
sexp <- function() { # Exponential Distribution Simulator
  show.mnsd <- show.both <- lambda <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(0,7)
        x <- seq(xr[1],xr[2],by=0.01)
        fx <- stats::dexp(x,lambda)
        Fx <- stats::pexp(x,lambda)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="Time",ylab="F(x)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
        }
      }, lambda=manipulate::slider(0.2,5,step=0.1,initial=1),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
}

#' @rdname simDistrib
#' @export
sf <- function() { # F Distribution Simulator
  show.mnsd <- show.both <- ndf <- ddf <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(0,6)
        x <- seq(0.01,xr[2],by=0.01)
        fx <- stats::df(x,ndf,ddf)
        Fx <- stats::pf(x,ndf,ddf)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="F",ylab="f(f)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="F",ylab="F(f)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="F",ylab="f(f)",main="PDF",xlim=xr,show.mnsd)
        }
      }, ndf=manipulate::slider(1,10,step=1,initial=1,label="Numerator df"),
         ddf=manipulate::slider(5,50,step=1,initial=5,label="Denominator df"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
sgamma <- function() {  # Gamma Distribution Simulator
  show.mnsd <- show.both <- alpha <- lambda <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(0,15)
        x <- seq(0.01,xr[2],by=0.01)
        fx <- stats::dgamma(x,alpha,lambda)
        Fx <- stats::pgamma(x,alpha,lambda)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="Time",ylab="F(x)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
        }
      }, alpha=manipulate::slider(0.2,5,step=0.1,initial=1),
         lambda=manipulate::slider(0.2,5,step=0.1,initial=1),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
slnorm <- function() {  # Log-normal Distribution Simulator
  show.mnsd <- show.both <- mu <- sigma <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(0,25)
        x <- seq(xr[1],xr[2],by=0.01)
        fx <- stats::dlnorm(x,mu,sigma)
        Fx <- stats::plnorm(x,mu,sigma)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="X",ylab="F(x)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
        }
      }, mu=manipulate::slider(0,2,step=0.1,initial=0,label="log mu"),
         sigma=manipulate::slider(0.2,2,step=0.2,initial=1,label="log sigma"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
snorm <- function() { # Normal Distribution Simulator
  show.mnsd <- show.both <- mu <- sigma <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        xr <- c(-10,10)
        x <- seq(xr[1],xr[2],by=0.01)
        fx <- stats::dnorm(x,mu,sigma)
        Fx <- stats::pnorm(x,mu,sigma)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
          cCDF.plot(x,Fx,xlab="X",ylab="F(x)",main="CDF",xlim=xr)
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
        }
      }, mu=manipulate::slider(-5,5,step=0.2,initial=0,label="mu"),
         sigma=manipulate::slider(0.2,2,step=0.2,initial=1,label="sigma"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
st <- function() { # t Distribution Simulator
  show.mnsd <- show.both <- show.norm <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        yr <- c(0,0.4)
        xr <- c(-4,4)
        x <- seq(xr[1],xr[2],by=0.01)
        fx <- stats::dt(x,df)
        fx.norm <- stats::dnorm(x,0,1)
        Fx <- stats::pt(x,df)
        Fx.norm <- stats::pnorm(x,0,1)
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          cPDF.plot(x,fx,xlab="t",ylab="f(t)",main="PDF",xlim=xr,ylim=yr,show.mnsd)
          if (show.norm) graphics::lines(x,fx.norm,col="gray50")
          cCDF.plot(x,Fx,xlab="t",ylab="F(t)",main="CDF",xlim=xr)
          if (show.norm) graphics::lines(x,Fx.norm,col="gray50")
        } else {
          graphics::par(mfcol=c(1,1))
          cPDF.plot(x,fx,xlab="t",ylab="f(t)",main="PDF",xlim=xr,ylim=yr,show.mnsd)
          if (show.norm) graphics::lines(x,fx.norm,col="gray50")
        }
      }, df=manipulate::slider(1,50,step=1,initial=2),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD"),
         show.norm=manipulate::checkbox(FALSE,label="Show standard normal")
    ) # end manipulate
  }
  graphics::par(old.par)
}




#' @rdname simDistrib
#' @export
sbinom <- function() { # Binomial Distribution Simulator
  show.mnsd <- show.both <- n <- p <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        x <- 0:n
        fx <- stats::dbinom(x,n,p)
        Fx <- stats::pbinom(x,n,p)
        xlbl <- "Number of Successes"
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
          dCDF.plot(x,Fx,xlab=xlbl,ylab="F(x)",main="CDF")
        } else {
          graphics::par(mfcol=c(1,1))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
        }
      }, n=manipulate::slider(1,100,step=1,initial=10,label="Sample Size (n)"),
         p=manipulate::slider(0.01,0.99,step=0.01,initial=0.5,label="PR(Success)"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
sgeom <- function() {  # Geometric Distribution Simulator
  show.mnsd <- show.both <- p <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        x <- 0:100
        fx <- stats::dgeom(x,p)
        x2keep <- which(fx>0.001)
        x <- x[c(x2keep,max(x2keep)+1)]
        fx <- c(fx[x2keep],0)
        Fx <- stats::pgeom(x,p)
        xlbl <- "Number of Failures"
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
          dCDF.plot(x,Fx,xlab=xlbl,ylab="F(x)",main="CDF")
        } else {
          graphics::par(mfcol=c(1,1))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
        }
      }, p=manipulate::slider(0.05,0.90,step=0.01,initial=0.5,label="PR(Success)"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
shyper <- function() { # Hypergeometric Distribution Simulator
  show.mnsd <- show.both <- n <- M <- Mf <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        x <- 0:(M+1)
        fx <- stats::dhyper(x,M,Mf,n)
        Fx <- stats::phyper(x,M,Mf,n)
        xlbl <- "Number of Successes"
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
          dCDF.plot(x,Fx,xlab=xlbl,ylab="F(x)",main="CDF")
        } else {
          graphics::par(mfcol=c(1,1))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
        }
      }, M=manipulate::slider(10,100,step=1,initial=10,label="Successes in Popn (M)"),
         Mf=manipulate::slider(50,200,step=1,initial=150,label="Failures in Popn (N-M)"),
         n=manipulate::slider(2,100,step=1,initial=30,label="Sample Size (n)"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
snbinom <- function() { # Negative Binomial Distribution Simulator
  show.mnsd <- show.both <- r <- p <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        x <- 0:(r*300)
        fx <- stats::dnbinom(x,r,p)
        x2keep <- which(fx>0.001)
        x <- x[c(x2keep,max(x2keep)+1)]
        fx <- c(fx[x2keep],0)
        Fx <- stats::pnbinom(x,r,p)
        xlbl <- "Number of Failures"
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
          dCDF.plot(x,Fx,xlab=xlbl,ylab="F(x)",main="CDF")
        } else {
          graphics::par(mfcol=c(1,1))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
        }
      }, r=manipulate::slider(1,10,step=1,initial=2,label="Successes to Observe (r)"),
         p=manipulate::slider(0.05,0.9,step=0.01,initial=0.5,label="PR(Success)"),
         show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
         show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}

#' @rdname simDistrib
#' @export
spois <- function() {  # Poisson Distribution Simulator
  show.mnsd <- show.both <- lambda <- NULL   # work-around no visible binding
  old.par <- graphics::par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0))
  if (iCheckRStudio() & iChk4Namespace("manipulate")) {
    manipulate::manipulate(
      { # make data
        x <- 0:200
        fx <- stats::dpois(x,lambda)
        x2keep <- which(fx>0.001)
        x <- x[c(x2keep,max(x2keep)+1)]
        fx <- c(fx[x2keep],0)
        Fx <- stats::ppois(x,lambda)
        xlbl <- "Number of Successes"
        # make graph
        if (show.both) {
          graphics::par(mfcol=c(1,2))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
          dCDF.plot(x,Fx,xlab=xlbl,ylab="F(x)",main="CDF")
        } else {
          graphics::par(mfcol=c(1,1))
          dPDF.plot(x,fx,xlab=xlbl,ylab="f(x)",main="PDF",show.mnsd)
        }
      }, lambda=manipulate::slider(1,30,step=1,initial=5),
      show.both=manipulate::checkbox(TRUE,label="Show PDF & CDF"),
      show.mnsd=manipulate::checkbox(TRUE,label="Show mean & SD")
    ) # end manipulate
  }
  graphics::par(old.par)
}
