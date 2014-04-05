#'Z test for known population standard deviation
#'
#'Compute the test of hypothesis and compute confidence interval on the mean of
#'a population when the standard deviation of the population is known.
#'
#'Most introductory statistical texts introduce inference by using the Z test
#'and Z based confidence intervals based on knowing the population standard
#'deviation.  Most statistical packages do not include functions to do Z tests
#'because the T test is usually more appropriate for real world situations.
#'This function is meant to be used during that short period of learning when
#'the student is learning about inference using Z procedures, but has not
#'learned the T based procedures yet.  Once the student has learned about the T
#'distribution the \code{t.test()} function should be used instead of this one
#'(but the syntax is very similar, so this function should be an appropriate
#'introductory step to learning \code{t.test()}).
#'
#'@param x Vector of data values.
#'@param mu Hypothesized mean of the population.
#'@param sd Known standard deviation of the population.
#'@param alternative Direction of the alternative hypothesis.
#'@param conf.level Confidence level for the interval computation.
#'@param \dots Additional arguments are silently ignored.
#'@return An object of class \code{htest} containing the results
#'@note This function should be used for learning only, real data should
#'generally use \code{t.test()}.
#'@author Greg Snow \email{greg.snow@@imail.org} with a slight modification (removed
#'the stdev argument) by the package author.
#'@seealso \code{\link{t.test}}
#'@keywords htest
#'@examples
#'x <- rnorm(25, 100, 5)
#'z.test(x, 99, 5)
#'
#'@export
z.test <- function (x,mu=0,sd,alternative=c("two.sided","less","greater"),conf.level=0.95,...) {
  if (missing(sd)) stop("You must specify a Standard Deviation of the population",call.=FALSE)
  alternative <- match.arg(alternative)
  n <- length(x)
  z <- (mean(x) - mu)/(sd/sqrt(n))
  out <- list(statistic = c(z = z))
  class(out) <- "htest"
  out$parameter <- c(n=n,`Std. Dev.`=sd,`SE of the sample mean` = sd/sqrt(n))
  out$p.value <- switch(alternative, two.sided=2*pnorm(abs(z),lower.tail=FALSE),
    less=pnorm(z), greater=pnorm(z,lower.tail=FALSE))
  out$conf.int <- switch(alternative,
    two.sided=mean(x)+c(-1,1)*qnorm(1-(1-conf.level)/2)*sd/sqrt(n),
    less = c(-Inf, mean(x)+qnorm(conf.level)*sd/sqrt(n)),
    greater = c(mean(x)-qnorm(conf.level)*sd/sqrt(n),Inf))
  attr(out$conf.int,"conf.level") <- conf.level
  out$estimate <- c(`mean of x`=mean(x))
  out$null.value <- c(mean=mu)
  out$alternative <- alternative
  out$method <- "One Sample z-test"
  out$data.name <- deparse(substitute(x))
  names(out$estimate) <- paste("mean of", out$data.name)
  out
}
