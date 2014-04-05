#'Extract the coefficient of determination from a linear model object.
#'
#'Extracts the coefficient of determination (i.e., \dQuote{r-squared} from a
#'linear model (i.e., \code{lm}) object.
#'
#'This is a convenience function for extracting the \code{r.squared} part from
#'\code{summary(lm)}.
#'
#'@param x An object saved from \code{lm}.
#'@param digits A number indicating the number of digits to round the returned
#'result to.
#'@param percent A logical indicating if the result should be returned as a
#'percentage (\code{=TRUE}) or as a proportion (\code{=FALSE}; default).
#'@return Returns a number representing the coefficient of determination as
#'either a proportion or percentage.
#'@keywords misc
#'@examples
#'require(FSA)  # for Mirex data
#'data(Mirex)
#'# Simple linear regression test HA:slope!=0.1
#'lm1 <- lm(mirex~weight, data=Mirex)
#'rSquared(lm1)
#'rSquared(lm1,digits=3)
#'rSquared(lm1,digits=1,percent=TRUE)
#'
#'@export
rSquared <- function(x,digits=getOption("digits"),percent=FALSE) {
  if (class(x)!="lm") stop(paste("'rSquared' only works if 'x' is an 'lm' object.\n Your 'x' is of the",class(x),"class"),call.=FALSE)
  r2 <- summary(x)$r.squared
  ifelse(percent,round(r2*100,digits),round(r2,digits))
}
