#' @title Modificaton of print.lm() to streamline output
#'  
#' @description Modifies the print.lm() function so that the output is less verbose.  For example, the call will not be re-printed unless asked for.  In addition, several \sQuote{extra} blank lines were removed.
#'  
#' @aliases print.lm
#' 
#' @param x An object of class \code{lm} or \code{summary.lm} to be printed.
#' @param digits A single numeric that indicates the number of digits to use.
#' @param show.call A single logical that indicates whether the call should be re-printed.
#' @param \dots Not implemented.
#' 
#' @return Invisibly returns the sent \code{x}.
#' 
#' @examples
#' ## from lm examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2,10,20, labels=c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' 
#' # the new print version of the lm object
#' lm.D9 <- lm(weight ~ group)
#' 
#' # the new print version of the summary.lm object
#' summary(lm.D9)
#' 
#' @method print lm
#' @export
print.lm <- function(x,digits=max(3,getOption("digits")-3),show.call=FALSE,...) {
  if (show.call) cat("Call:",paste(deparse(x$call),sep="\n",collapse="\n"),"\n\n",sep="")
  if (length(stats::coef(x))) {
    cat("Coefficients:\n")
    print.default(format(stats::coef(x),digits=digits),print.gap=2,quote=FALSE)
  }
  else cat("No coefficients\n")
  invisible(x)
}
