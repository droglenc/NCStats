#' Constructs confidence intervals assuming normal distribution.
#' 
#' Returns the confidence interval endpoints given an estimate, SE, and df assuming that the estimate follows a normal distribution so that the t-distribution can be used when constructing the CI.
#' 
#' @param est A value that estimates a parameter (i.e., a statistic)
#' @param SE The standard error of the estimate
#' @param obsdf The degrees-of-freedom
#' @param conf.level The level of confidence as a decimal
#' 
#' @return Returns a matrix containing the lower and upper values of the confidence interval.
#' 
#' @seealso \code{\link[FSA]{confint.nlsBoot}} in \pkg{FSA}.
#' 
#' @keywords htest
#' 
#' @examples
#' 
#' ci.t(2.96,0.32,14)
#' 
#' @export
ci.t <- function(est,SE,obsdf,conf.level=0.95) {
  hw <- stats::qt(0.5+conf.level/2,obsdf)*SE
  res <- cbind(est-hw,est+hw)
  colnames(res) <- c(paste(paste0(round(100*conf.level,1),"%"),c("LCI","UCI")))
  res
}
