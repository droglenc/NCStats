#'Multiple Simulations from Normal Distributions
#'
#'Random generation from multiple normal distributions with potentially
#'different means and standard deviations.
#'
#'All of \code{n}, \code{mean}, \code{sd}, and \code{grp.labels} must be of the
#'same length.
#'
#'If \code{digits} is non-null and \code{exact=TRUE} the resulting quantitative
#'data will only be approximately exact (due to the rounding).
#'
#'@param n vector of number of observations.
#'@param mean vector of means.
#'@param sd vector of standard deviations.
#'@param exact a logical that indicates whether the resulting vector of random
#'numbers will have the exact mean and standard deviation supplied in
#'\code{mean} and \code{sd}.
#'@param grp.labels Labels for the levels representing the different groups.
#'@param var.labels Labels for or names for the columns of the resulting data
#'frame.
#'@param digits A number of digits to which the numeric data should be
#'rounded.
#'@return A data frame with two columns is returned.  The first columns is the
#'random normal deviates and the second column are the group levels.
#'@note This function can be used to generate \dQuote{realistic} data when one
#'knows the sample size, mean, and standard deviation for several groups and it
#'can be assumed that the data in each group follows a normal distribution.
#'Thus, this function can be used to generate \dQuote{actual} data for, for
#'example, one-way and two-way ANOVA from summaries of group sample sizes,
#'means, and standard deviations.  Note that standard deviations can often be
#'estimated by \dQuote{back-calculating} from given standard errors or
#'confidence intervals.
#'@seealso \code{\link{rnorm}}.
#'@keywords distribution
#'@examples
#'require(FSA)  # for Summarize()
#'# using default names
#'rand.data <- mrnorm(n=c(10,15,20),mean=c(10,15,15),sd=c(3,4,5))
#'Summarize(measure~group,data=rand.data)
#'  
#'# using custom names
#'rand.data1 <- mrnorm(n=c(10,15),mean=c(10,15),sd=c(3,4),
#'  grp.labels=c("First","Second"),var.labels=c("Y","X"))
#'Summarize(Y~X,data=rand.data1)
#'
#'@export
mrnorm <- function(n,mean,sd,exact=TRUE,grp.labels=LETTERS[1:length(n)],var.labels=c("measure","group"),digits=NULL) {
  if (any(diff(c(length(n),length(mean),length(sd),length(grp.labels)))!=0)) stop("One of n, mean, sd, grp.labels is of different length.",call.=FALSE)
  d <- grps <- NULL
  for (i in 1:length(n))  {
    d1 <- rnorm(n[i],mean[i],sd[i])
    if (exact) {
      z <- (d1-mean(d1))/sd(d1)
      d1 <- sd[i]*z+mean[i]
    }
    d <- c(d,d1)
    grps <- c(grps,rep(grp.labels[i],each=n[i]))
  }
  if (!is.null(digits)) d <- round(d,digits)
  ans <- data.frame(d,grps)
  colnames(ans) <- var.labels
  ans
}
