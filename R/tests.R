#' @title Anderson-Darling test for normality
#' 
#' @description Performs the Anderson-Darling test for the composite hypothesis of normality. This is simply a camel-case wrapper to the \code{\link[nortest]{ad.test}} in \pkg{nortest}.
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
#' adTest(rnorm(100,mean=5,sd=3))
#' adTest(runif(100,min=2,max=4))
#' 
#' @export
adTest <- function(x) if (iChk4Namespace("nortest")) nortest::ad.test(x)




#' @title Perform Levene's test for homogeneity of variances
#'
#' @description Perform Levene's test for homogeneity of variances
#'
#' @param \dots Arguments to pass through to \code{\link[car]{leveneTest}} in \pkg{car}.
#'
#' @return See help for \code{\link[car]{leveneTest}} in \pkg{car}.
#'
#' @seealso \code{\link[car]{leveneTest}} in \pkg{car}.
#' 
#' @note This is a simple pass-through to  \code{\link[car]{leveneTest}} in \pkg{car} for those of us that always put the \sQuote{s} on \sQuote{Levenes}.
#' 
#' @keywords misc
#' 
#' @export
levenesTest <- function(...) car::leveneTest(...)




#' @title Performs a test for outliers
#'
#' @description Performs a test for outliers
#'
#' @param \dots Arguments to pass through to \code{\link[car]{outlierTest}} in \pkg{car}.
#'
#' @return See help for \code{\link[car]{outlierTest}} in \pkg{car}.
#'
#' @seealso \code{\link[car]{outlierTest}} in \pkg{car}.
#' 
#' @note This is a simple pass-through to  \code{\link[car]{outlierTest}} in \pkg{car}.
#' 
#' @keywords misc
#' 
#' @export
outlierTest <- function(...) car::outlierTest(...)




#' @title Z test for known population standard deviation
#' 
#' @description Compute the test of hypothesis and compute confidence interval on the mean of a population when the standard deviation of the population is known.
#' 
#' @details Most introductory statistical texts introduce inference by using the Z test and Z based confidence intervals based on knowing the population standard deviation. Most statistical packages do not include functions to do Z tests because the T test is usually more appropriate for real world situations. This function is meant to be used during that short period of learning when the student is learning about inference using Z procedures, but has not learned the T based procedures yet. Once the student has learned about the T distribution the \code{\link[stats]{t.test}} function should be used instead of this one (but the syntax is very similar, so this function should be an appropriate introductory step to learning \code{\link[stats]{t.test}}).
#' 
#' @param x Vector of data values
#' @param mu Hypothesized mean of the population
#' @param sd Known standard deviation of the population
#' @param alternative Direction of the alternative hypothesis
#' @param conf.level Confidence level for the interval computation
#' @param \dots Additional arguments are silently ignored
#' 
#' @return An object of class \code{htest} containing the results
#' 
#' @note This function should be used for learning only, real data should generally use \code{\link[stats]{t.test}}.
#' 
#' @author This is \code{\link[TeachingDemos]{z.test}} in \pkg{TeachingDemos} but imported and exported here.
#' 
#' @seealso \code{\link{t.test}}
#' 
#' @keywords htest
#' 
#' @examples
#' z.test(rnorm(25,100,5),99,5)
#' 
#' @export
#' @name z.test
#' @rdname z.test
#' @importFrom TeachingDemos z.test
#' @export
NULL