#'Demonstrates the shape of the PDF and CDF for the given distribution.
#'
#'Dynamically demonstrates the effect of parameter choices on the shape of the
#'PDF and CDF for the given distribution.
#'
#'A graphic will be produced that is connected to a slider bar where the user
#'can change the parameters of the distribution.  The real-time updating of the
#'graphic allows the user to determine the effect of changing each parameter on
#'the shape, mean, and standard deviation of the distribution.
#'
#'@aliases sbeta sbinom schisq sexp sf sgamma sgeom shyper slnorm snbinom snorm
#'spois st
#'@param show.both logical; if \code{TRUE} (default) then show both the PDF and
#'CDF, else just the PDF.
#'@param show.mnsd logical; if \code{TRUE} (default) then show the mean and
#'standard deviation on the PDF.
#'@return None, but a dynamic graphic with slider bars will be produced.
#'@seealso \code{vis.binom}, \code{vis.gamma}, \code{vis.normal},and
#'\code{vis.t} of the \code{TeachingDemos} package.  Also see
#'\code{\link{dbeta}}, \code{\link{dbinom}}, \code{\link{dchisq}}, etc.
#'@keywords hplot distribution dynamic
#'@examples
#'if(interactive()) {
#'
#'sbeta()
#'sbinom()
#'schisq()
#'sexp()
#'sf()
#'sgamma()
#'sgeom()
#'shyper()
#'slnorm()
#'snbinom()
#'snorm()
#'spois()
#'st()
#'
#'}
#'
#'@rdname simDistrib
#'@export sbeta
#'
sbeta <- function(show.both=TRUE,show.mnsd=TRUE) {                       # Beta Distribution Simulator
  refresh <- function(...) {
    alpha <- relax::slider(no=1)
    beta <- relax::slider(no=2)
    xr <- c(0,1)
    x <- seq(0.01,0.99,by=0.01)
    fx <- dbeta(x,alpha,beta)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
      cCDF.plot(x,pbeta(x,alpha,beta),xlab="X",ylab="F(x)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("alpha","beta"),
             sl.mins=    c(    0.2,    0.2),
             sl.maxs=    c(    5.0,    5.0),
             sl.deltas=  c(    0.1,    0.1),
             sl.defaults=c(    1.0,    1.0),
             title = "Beta Distribution Simulator")
  }
}

#'@rdname simDistrib
#'@export sbinom
sbinom <- function(show.both=TRUE,show.mnsd=TRUE) {                     # Binomial Distribution Simulator
  refresh <- function(...) {
    n <- relax::slider(no=1)
    p <- relax::slider(no=2)
    x <- 0:n
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      dPDF.plot(x,dbinom(x,n,p),xlab="Number of Successes",ylab="f(x)",main="PDF",show.mnsd)
      dCDF.plot(x,pbinom(x,n,p),xlab="Number of Successes",ylab="F(x)",main="CDF")
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      dPDF.plot(x,dbinom(x,n,p),xlab="Number of Successes",ylab="f(x)",main="PDF",show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("Sample Size (n)", "PR(Success)"),
             sl.mins=    c(                1,             0.01),
             sl.maxs=    c(              100,             0.99),
             sl.deltas=  c(                1,             0.01),
             sl.defaults=c(               10,             0.50),
             title = "Binomial Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export schisq
schisq <- function(show.both=TRUE,show.mnsd=TRUE) {                         # chi-square Distribution Simulator
  refresh <- function(...) {
    df <- relax::slider(no=1)
    xr <- c(0,50)
    x <- seq(0.01,xr[2],by=0.01)
    fx <- dchisq(x,df)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="Chi-Square",ylab="f(chi)",xlim=xr,main="PDF",show.mnsd)
      cCDF.plot(x,pchisq(x,df),xlab="Chi-Square",ylab="F(chi)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="Chi-Square",ylab="f(chi)",xlim=xr,main="PDF",show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("df"),
             sl.mins=    c(   1),
             sl.maxs=    c(  20),
             sl.deltas=  c(   1),
             sl.defaults=c(   5),
             title = "Student's t Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export sexp
sexp <- function(show.both=TRUE,show.mnsd=TRUE) {                       # Exponential Distribution Simulator
  refresh <- function(...) {
    lambda <- relax::slider(no=1)
    xr <- c(0,7)
    x <- seq(xr[1],xr[2],by=0.01)
    fx <- dexp(x,lambda)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
      cCDF.plot(x,pexp(x,lambda),xlab="Time",ylab="F(x)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("Lambda"),
             sl.mins=    c(     0.2),
             sl.maxs=    c(     5.0),
             sl.deltas=  c(     0.1),
             sl.defaults=c(     1.0),
             title = "Exponential Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export sf
sf <- function(show.both=TRUE,show.mnsd=TRUE) {                       # F Distribution Simulator
  refresh <- function(...) {
    ndf <- relax::slider(no=1)
    ddf <- relax::slider(no=2)
    xr <- c(0,6)
    x <- seq(0.01,xr[2],by=0.01)
    fx <- df(x,ndf,ddf)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="F",ylab="f(F)",main="PDF",xlim=xr,show.mnsd)
      cCDF.plot(x,pf(x,ndf,ddf),xlab="F",ylab="F(F)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="F",ylab="f(F)",main="PDF",xlim=xr,show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("numerator df","denominator df"),
             sl.mins=    c(            1,                 5),
             sl.maxs=    c(           10,                50),
             sl.deltas=  c(            1,                 1),
             sl.defaults=c(            1,                 5),
             title = "F Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export sgamma
sgamma <- function(show.both=TRUE,show.mnsd=TRUE) {                       # Gamma Distribution Simulator
  refresh <- function(...) {
    alpha <- relax::slider(no=1)
    lambda <- relax::slider(no=2)
    xr <- c(0,15)
    x <- seq(0.01,xr[2],by=0.01)
    fx <- dgamma(x,alpha,lambda)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
      cCDF.plot(x,pgamma(x,alpha,lambda),xlab="Time",ylab="F(x)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="Time",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("alpha","Lambda"),
             sl.mins=    c(    0.2,      0.2),
             sl.maxs=    c(    5.0,      5.0),
             sl.deltas=  c(    0.1,      0.1),
             sl.defaults=c(    1.0,      1.0),
             title = "Gamma Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export sgeom
sgeom <- function(show.both=TRUE,show.mnsd=TRUE) {                      # Geometric Distribution Simulator
  refresh <- function(...) {
    p <- relax::slider(no=1)
    x <- 0:300
    fx <- dgeom(x,p)
    x2keep <- which(fx>0.001)
    x1 <- x[c(x2keep,max(x2keep)+1)]; fx1 <- c(fx[x2keep],0)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      dPDF.plot(x1,fx1,xlab="Number of Failures",ylab="f(x)",main="PDF",show.mnsd)
      dCDF.plot(x1,pgeom(x1,p),xlab="Number of Failures",ylab="F(x)",main="CDF")
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      dPDF.plot(x1,fx1,xlab="Number of Failures",ylab="f(x)",main="PDF",show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("PR(Success)"),
             sl.mins=    c(          0.05),
             sl.maxs=    c(          0.90),
             sl.deltas=  c(          0.01),
             sl.defaults=c(          0.50),
             title = "Geometric Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export shyper
shyper <- function(show.both=TRUE,show.mnsd=TRUE) {                     # Hypergeometric Distribution Simulator
  refresh <- function(...) {
    M <- relax::slider(no=1)
    N <- M+relax::slider(no=2)
    n <- relax::slider(no=3)
    if (n > N) stop("Sample size (n) can not be greater than population size (N)",call.=FALSE)
    x <- 0:(M+1)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      dPDF.plot(x,dhyper(x,M,N-M,n),xlab="Number of Successs",ylab="f(x)",main="PDF",show.mnsd)
      dCDF.plot(x,phyper(x,M,N-M,n),xlab="Number of Successs",ylab="F(x)",main="CDF")
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      dPDF.plot(x,dhyper(x,M,N-M,n),xlab="Number of Successs",ylab="f(x)",main="PDF",show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("Successes in Popn (M)","Failures in Popn (N-M)","Sample Size (n)"),
             sl.mins=    c(                     10,                      10,                2),
             sl.maxs=    c(                    200,                     200,              100),
             sl.deltas=  c(                      1,                       1,                1),
             sl.defaults=c(                    100,                     100,               10),
             title = "Hypergeometric Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export slnorm
slnorm <- function(show.both=TRUE,show.mnsd=TRUE) {                        # Normal Distribution Simulator
  refresh <- function(...) {
    mu <- relax::slider(no=1)
    sigma <- relax::slider(no=2)
    xr <- c(0,25)
    x <- seq(xr[1],xr[2],by=0.01)
    fx <- dlnorm(x,mu,sigma)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
      cCDF.plot(x,pnorm(x,mu,sigma),xlab="X",ylab="F(x)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("log mu","log sigma"),
             sl.mins=    c(  0,      0.2),
             sl.maxs=    c(  2,      2  ),
             sl.deltas=  c(0.1,      0.2),
             sl.defaults=c(  0,      1  ),
             title = "Lognormal Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export snbinom
snbinom <- function(show.both=TRUE,show.mnsd=TRUE) {                    # Negative Binomial Distribution Simulator
  refresh <- function(...) {
    r <- relax::slider(no=1)
    p <- relax::slider(no=2)
    x <- 0:(r*300)
    fx <- dnbinom(x,r,p)
    x2keep <- which(fx>0.001)
    x1 <- x[c(x2keep,max(x2keep)+1)]; fx1 <- c(fx[x2keep],0)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      dPDF.plot(x1,fx1,xlab="Number of Failures",ylab="f(x)",main="PDF",show.mnsd)
      dCDF.plot(x1,pnbinom(x1,r,p),xlab="Number of Failures",ylab="F(x)",main="CDF")
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      dPDF.plot(x1,fx1,xlab="Number of Failures",ylab="f(x)",main="PDF",show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("Successes to Observe (r)","PR(Success)"),
             sl.mins=    c(                         1,         0.05),
             sl.maxs=    c(                        10,         0.90),
             sl.deltas=  c(                         1,         0.01),
             sl.defaults=c(                         2,         0.50),
             title = "Negative Binomial Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export snorm
snorm <- function(show.both=TRUE,show.mnsd=TRUE) {                        # Normal Distribution Simulator
  refresh <- function(...) {
    mu <- relax::slider(no=1)
    sigma <- relax::slider(no=2)
    xr <- c(-10,10)
    x <- seq(xr[1],xr[2],by=0.01)
    fx <- dnorm(x,mu,sigma)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
      cCDF.plot(x,pnorm(x,mu,sigma),xlab="X",ylab="F(x)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="X",ylab="f(x)",main="PDF",xlim=xr,show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("mu","sigma"),
             sl.mins=    c( -5,      0.2),
             sl.maxs=    c(  5,      2  ),
             sl.deltas=  c(0.2,      0.2),
             sl.defaults=c(  0,      1  ),
             title = "Normal Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export spois
spois <- function(show.both=TRUE,show.mnsd=TRUE) {                      # Poisson Distribution Simulator
  refresh <- function(...) {
    lambda <- relax::slider(no=1)
    x <- 0:200
    fx <- dpois(x,lambda)
    x2keep <- which(fx>0.001)
    x1 <- x[c(x2keep,max(x2keep)+1)]; fx1 <- c(fx[x2keep],0)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      dPDF.plot(x1,fx1,xlab="Number of Successes",ylab="f(x)",main="PDF",show.mnsd)
      dCDF.plot(x1,ppois(x1,lambda),xlab="Number of Successes",ylab="F(x)",main="CDF")
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      dPDF.plot(x1,fx1,xlab="Number of Successes",ylab="f(x)",main="PDF",show.mnsd)
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("Lambda"),
             sl.mins=    c(       1),
             sl.maxs=    c(      30),
             sl.deltas=  c(       1),
             sl.defaults=c(       5),
             title = "Poisson Distribution Simulator",pos.of.panel="left")
  }
}

#'@rdname simDistrib
#'@export st
st <- function(show.both=TRUE,show.mnsd=TRUE) {                         # t Distribution Simulator
  refresh <- function(...) {
    df <- relax::slider(no=1)
    xr <- c(-4,4)
    x <- seq(xr[1],xr[2],by=0.01)
    fx <- dt(x,df)
    fx.norm <- dnorm(x,0,1)
    if (show.both) {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0), mfcol=c(1,2)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="t",ylab="f(t)",main="PDF",xlim=xr,ylim=c(0,max(fx.norm)),show.mnsd)
      lines(x,fx.norm,col="gray")
      cCDF.plot(x,pt(x,df),xlab="t",ylab="F(t)",main="CDF",xlim=xr)
    } else {
      old.par <- par(mar=c(3.5,3.5,3.5,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
      cPDF.plot(x,fx,xlab="t",ylab="f(t)",main="PDF",xlim=xr,ylim=c(0,max(fx.norm)),show.mnsd)
      lines(x,fx.norm,col="gray")
    }
  } ## end internal function
  if (iChk4Namespace("relax")) {
    relax::gslider(refresh,prompt=TRUE, hscale=2,
             sl.names=   c("df"),
             sl.mins=    c(   1),
             sl.maxs=    c(  50),
             sl.deltas=  c(   1),
             sl.defaults=c(   5),
             title = "Student's t Distribution Simulator",pos.of.panel="left")
  }
} 