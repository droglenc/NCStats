#' Z test for known population standard deviation
#' 
#' Compute the test of hypothesis and compute confidence interval on the mean of a population when the standard deviation of the population is known.
#' 
#' Most introductory statistical texts introduce inference by using the Z test and Z based confidence intervals based on knowing the population standard deviation.  Most statistical packages do not include functions to do Z tests because the T test is usually more appropriate for real world situations.  This function is meant to be used during that short period of learning when the student is learning about inference using Z procedures, but has not learned the T based procedures yet.  Once the student has learned about the T distribution the \code{\link[stats]{t.test}} function should be used instead of this one (but the syntax is very similar, so this function should be an appropriate introductory step to learning \code{\link[stats]{t.test}}).
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
#' @author This is \code{\link[stats]{z.test}} in \pkg{TeachingDemos} but imported and exported here.
#' 
#' @seealso \code{\link{t.test}}
#' 
#' @keywords htest
#' 
#' @examples
#' x <- rnorm(25, 100, 5)
#' z.test(x, 99, 5)
#' 
#' @export
#' @name z.test
#' @rdname z.test
#' @importFrom TeachingDemos z.test
#' @export
NULL