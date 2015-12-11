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
# Check if a required namespace can be loaded and, if not, send
#   an error message.
##################################################################
iChk4Namespace <- function(pkg) {
  res <- requireNamespace(pkg,quietly=TRUE)
  if (!res) stop(paste0("The '",pkg," package must be installed."))
  res
}


##################################################################
# Try to determine if RStudio is being used.
##################################################################
iCheckRStudio <- function () "tools:rstudio" %in% search()


##################################################################
# Internal functions used in distrib().
##################################################################
c.region <- function(xval,x,fx,lower.tail,area,plot,show.ans,
                     shade.col,lbl.col,show.lbl,cex.ans=1,add=FALSE,...) {
  if (lower.tail) {
    x.shade <- x[x<=xval]
    y.shade <- fx[x<=xval]
    x.shade <- c(x.shade,xval,xval,min(x))
    ## extra [1] is for times when xval is in x twice
    y.shade <- c(y.shade,fx[x==xval][1],0,0)
  } else {
    x.shade <- x[x>=xval]
    y.shade <- fx[x>=xval]
    x.shade <- c(xval,x.shade,max(x),xval)
    ## extra [1] is for times when xval is in x twice
    y.shade <- c(fx[x==xval][1],y.shade,0,0)  
  }
  if (plot) {
    cPDF.plot(x,fx,show.mnsd=FALSE,...)
    if (show.lbl) graphics::axis(1,at=xval,labels=round(xval,3),
                                 line=0.9,tcl=1.5,fg=lbl.col,col.axis=lbl.col)
    graphics::polygon(x.shade,y.shade,col=shade.col,border=shade.col)
    graphics::lines(x,fx)
    if(show.ans) graphics::mtext(paste("Value =",round(xval,3),"; Area =",
                                       round(area,4)),line=0.2,col="red",cex=cex.ans)
  }
  if (add) {
    if (show.lbl) graphics::axis(1,at=xval,labels=round(xval,3),
                                 line=0.9,tcl=1.5,fg=lbl.col,col.axis=lbl.col)
    graphics::polygon(x.shade,y.shade,col=shade.col,border=shade.col)
    graphics::lines(x,fx)
    if(show.ans) graphics::mtext(paste("Value =",round(xval,3),"; Area =",
                                       round(area,4)),line=0.2,col="red",cex=cex.ans)
  }
  invisible(list(x=x,fx=fx,x.shade=x.shade,y.shade=y.shade))
}

cCDF.plot <- function(x,Fx,...) {
  graphics::plot(x,Fx,type="l",lwd=2,ylim=c(0,1),...)
}

cPDF.plot <- function(x,fx,show.mnsd=TRUE,...) {
  graphics::plot(x,fx,type="l",lwd=2,...)
  if (show.mnsd) {
    x.vals <- rep(x,round(10000*fx,0))
    mu <- mean(x.vals)
    sigma <- stats::sd(x.vals)
    graphics::mtext(paste("Mean = ",round(mu,1),", SD = ",round(sigma,1)),line=0.25,col="blue")
    d.max <- max(fx)
    graphics::lines(rep(mu,2),c(0,d.max),col="blue",lwd=2)
    graphics::lines(c(mu-sigma,mu+sigma),rep(0.6*d.max,2),col="blue",lwd=2)
  }
}

d.region <- function(xval,x,fx,region,area,reverse,plot,show.ans,
                     shade.col,lbl.col,cex.ans=1,...) {
  bp <- dPDF.plot(x,fx,show.mnsd=FALSE,...)
  switch(region,
         lower={
           xleft <- bp[x<=xval]-0.5
           xright <- xleft+1
           ytop <- fx[x<=xval]
           ybottom <- 0    
         },
         upper={
           ifelse(reverse,adjval <- xval-1, adjval <- xval)
           xleft <- bp[x>adjval]-0.5
           xright <- xleft+1
           ytop <- fx[x>adjval]
           ybottom <- 0
         },
         single={
           xleft <- bp[x==xval]-0.5
           xright <- xleft+1
           ytop <- fx[x==xval]
           ybottom <- 0
         } )
  if (plot) {
    graphics::rect(xleft,ybottom,xright,ytop,col=shade.col)
    if(show.ans) graphics::mtext(paste("Value =",xval,"; Probability =",
                                       round(area,4)),line=0.2,col="red",cex=cex.ans)  
  }
  invisible(list(xleft,xright,ybottom,ytop))
}

dCDF.plot <- function(x,Fx,...) {
  graphics::plot(x,Fx,pch=19,ylim=c(0,1),...)
  for (i in 1:(length(x)-1)) {
    graphics::lines(c(x[i+1],x[i+1]),c(Fx[i],Fx[i+1]),lwd=2,col="gray")
    graphics::lines(c(x[i],x[i+1]),c(Fx[i],Fx[i]),lwd=2)
  }
  graphics::points(x,Fx,pch=19)
}

dPDF.plot <- function(x,fx,show.mnsd=TRUE,col="gray90",...) {
  bp <- graphics::barplot(fx,space=0,col=col,...)
  graphics::axis(1,at=bp,labels=x)
  if (show.mnsd) {
    mu <- sum(x*fx)
    sigma <- sqrt(sum(((x-mu)^2)*fx))
    graphics::mtext(paste("Mean = ",round(mu,1),", SD = ",
                          round(sigma,1)),line=0.25,col="blue")
    mu.pos <- mu+bp[1]  # needed to place values on plot properly
    graphics::lines(rep(mu.pos,2),c(0,max(fx)),col="blue",lwd=2)
    graphics::lines(c(mu.pos-sigma,mu.pos+sigma),rep(0.6*max(fx),2),col="blue",lwd=2)
  }
  invisible(bp)
}


##############################################################
## Internal functions for handling formulas and types of lm
##    These are from FSA
##############################################################
iTypeoflm <- function(mdl) {
  if (any(class(mdl)!="lm")) stop("'iTypeoflm' only works with objects from 'lm()'.",call.=FALSE)
  tmp <- iHndlFormula(stats::formula(mdl),stats::model.frame(mdl))
  if (tmp$Enum==0) stop("Object must have one response and at least one explanatory variable",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("Response variable must be numeric",call.=FALSE)
  if (tmp$Etype=="factor") { #ANOVA
    if (tmp$EFactNum>2) stop("Function only works for one- or two-way ANOVA.")
    if (tmp$EFactNum==2) lmtype <- "TWOWAY"
    else lmtype <- "ONEWAY"
  } else { # not an anova
    if (tmp$Enum==1) lmtype <- "SLR"
    else if (tmp$Etype=="mixed") lmtype <- "IVR"
    else if (all(grepl(tmp$Enames[1],tmp$Enames[-1]))) lmtype <- "POLY"
    else lmtype <- "MLR"
  }
  tmp <- c(list(type=lmtype,mdl=mdl),tmp)
  class(tmp) <- c(lmtype,"list")
  tmp
}


iHndlFormula <- function(formula,data,expNumR=NULL,
                         expNumE=NULL,expNumENums=NULL,expNumEFacts=NULL) {
  mf <- stats::model.frame(formula,data=data,na.action=NULL)
  if (ncol(mf)==1) {
    # Only one variable in the model frame.  Return only the model.frame, name of 
    #   that variable, and it's class.
    return(list(mf=mf,vnum=1,vname=names(mf),vclass=class(mf[,1])))
  } else {
    # More than one variable in the formula.
    # Must identify if there is a LHS.
    ifelse(attr(stats::terms(formula),"response")==0,LHS <- FALSE, LHS <- TRUE)
    # See if more than one variable on LHS
    if (LHS) {
      fcLHS <- as.character(formula)[2]
      ifelse(any(c("*","+") %in% substring(fcLHS,1:nchar(fcLHS),1:nchar(fcLHS))),LHSgt1 <- TRUE, LHSgt1 <- FALSE)
      # STOP if there is more than one variable on LHS
      if (LHSgt1) stop("Function does not work with more than one variable on the LHS.",call.=FALSE)
      else {
        # There is a LHS and it has only one variable.
        Rpos <- Rnum <- 1
        Rname <- names(mf)[Rpos]
        Rmf <- mf[,Rpos]
        Rclass <- class(Rmf)
        Epos <- 2:ncol(mf)
        Enames <- names(mf)[Epos]
        Enum <- length(Enames)
        Emf <- mf[,Epos]
      }
    } else {
      # There is not a LHS
      Rnum <- 0
      Rpos <- Rname <- Rclass <- NULL
      Rmf <- NULL
      Emf <- mf
      Enames <- names(Emf)
      Enum <- length(Enames)
      Epos <- 1:Enum      
    }
    # find the class of each response and explanatory variable on the RHS
    if (Enum>0) ifelse(Enum==1,Eclass <- class(Emf), Eclass <- unlist(lapply(Emf,class)))
    # get positions of numeric and factor explanatory vars on RHS
    ENumPos <- which(Eclass %in% c("numeric","integer","AsIs"))
    EFactPos <- which(Eclass=="factor")
    # add one to positions if Rnum==1
    if (Rnum==1) {
      ENumPos <- ENumPos + 1
      EFactPos <- EFactPos + 1
    }
    # get number of numeric and number of factor explanatory vars on RHS
    ENumNum <- length(ENumPos)
    EFactNum <- length(EFactPos)
  }
  # Identify the type of data at hand
  Etype <- "mixed"
  if (ENumNum==0) Etype <- "factor"
  if (EFactNum==0) Etype <- "numeric"
  # Recreate the model frame data frame
  if (Rnum==0) df <- Emf
  else df <- data.frame(Rmf,Emf)
  names(df) <- c(Rname,Enames)
  # Check if the expected number of each type of variable was met
  metExpNumR <- metExpNumE <- metExpNumENums <- metExpNumEFacts <- NULL
  if (!is.null(expNumR)) ifelse(Rnum==expNumR,metExpNumR <- TRUE,metExpNumR <- FALSE)
  if (!is.null(expNumE)) ifelse(Enum==expNumE,metExpNumE <- TRUE,metExpNumE <- FALSE)
  if (!is.null(expNumENums)) ifelse(ENumNum==expNumENums,metExpNumENums <- TRUE,metExpNumENums <- FALSE)
  if (!is.null(expNumEFacts)) ifelse(EFactNum==expNumEFacts,metExpNumEFacts <- TRUE,metExpNumEFacts <- FALSE)
  # put it all together to return
  list(formula=formula,mf=df,vnum=Rnum+Enum,
       Rnum=Rnum,Rname=Rname,Rclass=Rclass,Rpos=Rpos,
       Etype=Etype,Enames=Enames,Eclass=Eclass,Enum=Enum,
       ENumNum=ENumNum,ENumPos=ENumPos,
       EFactNum=EFactNum,EFactPos=EFactPos,
       metExpNumR=metExpNumR,metExpNumE=metExpNumE,
       metExpNumENums=metExpNumENums,metExpNumEFacts=metExpNumEFacts)
}
