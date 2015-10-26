#' Anderson-Darling test for normality
#' 
#' Performs the Anderson-Darling test for the composite hypothesis of normality.  This is simply a camel-case wrapper to the \code{\link[nortest]{ad.test}} in \pkg{nortest}.
#' 
#' @param x a numeric vector of data values, the number of which must be greater
#' than 7. Missing values are allowed.
#' 
#' @return A list with class "htest" containing the following components:
#'   \itemize{
#'     \item statistic the value of the Anderson-Darling statistic.
#'     \item p.value the p-value for the test.
#'     \item method the character string \dQuote{Anderson-Darling normality test}.
#'     \item data.name a character string giving the name(s) of the data.
#' }
#' 
#' @note This is exactly the same as \code{\link[nortest]{ad.test}} in \pkg{nortest} package except that it uses \sQuote{camel-case} naming conventions.
#' 
#' @keywords htest
#' 
#' @examples
#' adTest(rnorm(100, mean = 5, sd = 3))
#' adTest(runif(100, min = 2, max = 4))
#' 
#' @export
adTest <- function(x) {
  if (!requireNamespace("nortest")) stop("'adTest' requires the 'nortest' package to be installed.",call.=FALSE)
  nortest::ad.test(x)
}
