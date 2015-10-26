#' Shows the steps in the manual calculation of the standard deviation.
#' 
#' Shows the steps in the manual calculation of the standard deviation.
#' 
#' @aliases sdCalc print.sdCalc
#' 
#' @param x A numeric vector
#' @param digits A numeric indicating the number of decimals to round the numeric summaries to.  If left at \code{NULL} (default) then the number of digits will be obtained from \code{getOption('digits')}
#' @param \dots Other arguments to the generic \code{print} functions (not currently used)
#' 
#' @return A list containing the sample size (\code{n}), sample mean (\code{mean}), data.frame of intermediate calculations (\code{tbl}), and number of digits to print (\code{digits))}. 
#' 
#' @note This function shows a table of intermediate output in the calculation of the standard deviation.  Used purely to demonstrate the hand-calculation of the standard deviation.  Use \code{\link[stats]{sd}} to actually compute the standard deviation.
#' 
#' @seealso \code{\link[stats]{sd}}
#' 
#' @keywords misc
#' 
#' @examples
#' ## Numeric vector
#' y <- runif(8)
#' # typical computation
#' sd(y)   
#' # this function           
#' sdCalc(y)            
#' # this function, controlling the number of digits
#' sdCalc(y,digits=4)
#' 
#' @rdname sdCalc
#' @export
sdCalc <- function(x,digits=getOption("digits")) {
  # make sure the vector is numeric
  if (!is.numeric(x)) stop("x must be numeric to compute the sd.",call.=FALSE)
  # remove missing values
  x1 <- x[!is.na(x)]
  # calculate parts
  n <- length(x1)
  xbar <- mean(x1)
  diffs <- x1-mean(x1)
  diffs.sq <- diffs^2
  df <- data.frame(x=x1,diffs,diffs.sq)
  df[nrow(df)+1,] <- c(sum(df[,1]),sum(df[,2]),sum(df[,3]))
  rownames(df)[nrow(df)] <- "sum"
  res <- list(n=n,mean=xbar,tbl=df,digits=digits)
  class(res) <- "sdCalc"
  res
}

#' @rdname sdCalc
#' @method print sdCalc
#' @export
print.sdCalc <- function(x,...) {
  cat("Demonstration of parts of a std. dev. calculation.\n\n")
  print(round(x$tbl,x$digits))
  cat("\nMean = x-bar =",round(x$tbl[nrow(x$tbl),"x"],x$digits),"/",x$n,"=",round(x$mean,x$digits),"\n")
  vrnc <- x$tbl[nrow(x$tbl),"diffs.sq"]/(x$n-1)
  cat("\nVariance = s^2 =",round(x$tbl[nrow(x$tbl),"diffs.sq"],x$digits),"/",x$n-1,"=",round(vrnc,x$digits),"\n")
  cat("\nStd. Dev = s = sqrt(",round(vrnc,x$digits),") = ",round(sqrt(vrnc),x$digits),"\n",sep="")
}
