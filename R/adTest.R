#'Anderson-Darling test for normality
#'
#'Performs the Anderson-Darling test for the composite hypothesis of normality,
#'see e.g. Thode (2002, Sec. 5.1.4).  This is the exact same coding as in the
#'nortest package.
#'
#'The Anderson-Darling test is an EDF omnibus test for the composite hypothesis
#'of normality.  The test statistic is \deqn{ A = -n -\frac{1}{n}
#'\sum_{i=1}^{n} [2i-1] [\ln(p_{(i)}) + \ln(1 - p_{(n-i+1)})], } where
#'\eqn{p_{(i)} = \Phi([x_{(i)} - \overline{x}]/s)}. Here, \eqn{\Phi} is the
#'cumulative distribution function of the standard normal distribution, and
#'\eqn{\overline{x}} and \eqn{s} are mean and standard deviation of the data
#'values.  The p-value is computed from the modified statistic \eqn{Z=A (1.0 +
#'0.75/n +2.25/n^{2})}\ according to Table 4.9 in Stephens (1986).
#'
#'@param x a numeric vector of data values, the number of which must be greater
#'than 7. Missing values are allowed.
#'@return A list with class "htest" containing the following components:
#'\itemize{
#'\item statistic the value of the Anderson-Darling statistic.
#'\item p.value the p-value for the test.
#'\item method the character string \dQuote{Anderson-Darling normality test}.
#'\item data.name a character string giving the name(s) of the data.
#'}
#'@note This is exactly the same as \code{ad.test} from the \code{nortest}
#'package.  I (Derek Ogle) copied it to the NCStats package so that I could use
#'\sQuote{camel-case} naming conventions and remove the dependency (for a
#'single reason) on the \code{nortest} package.
#'
#'The Anderson-Darling test is the recommended EDF test by Stephens (1986).
#'Compared to the Cramer-von Mises test (as second choice) it gives more weight
#'to the tails of the distribution.
#'@author Juergen Gross
#'@seealso \code{shapiro.test} for performing the Shapiro-Wilk test for
#'normality.  See \code{ad.test}, \code{cvm.test}, \code{lillie.test},
#'\code{pearson.test}, and \code{sf.test} in \code{nortest} package for
#'performing further tests for normality.  See \code{qqnorm} for producing a
#'normal quantile-quantile plot.
#'@references Stephens, M.A. (1986): Tests based on EDF statistics. In:
#'D'Agostino, R.B. and Stephens, M.A., eds.: Goodness-of-Fit Techniques.
#'Marcel Dekker, New York.
#'
#'Thode Jr., H.C. (2002): Testing for Normality. Marcel Dekker, New York.
#'@keywords htest
#'@examples
#'adTest(rnorm(100, mean = 5, sd = 3))
#'adTest(runif(100, min = 2, max = 4))
#'
#'@export
adTest <- function (x) {
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 8) stop("sample size must be greater than 7",call.=FALSE)
  p <- pnorm((x - mean(x))/sd(x))
  h <- (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
  A <- -n - mean(h)
  AA <- (1 + 0.75/n + 2.25/n^2) * A
  if (AA < 0.2) { pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2) }
    else if (AA < 0.34) { pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2) }
      else if (AA < 0.6) { pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2) }
        else { pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2) }
  RVAL <- list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test", data.name = DNAME)
  class(RVAL) <- "htest"
  RVAL
}
