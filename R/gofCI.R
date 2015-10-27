#' @title Pseudo- confidence intervals for multinomial levels.
#' 
#' @description Pseudo- confidence intervals for the proportions found in the multinomial levels of a goodness-of-fit test computed using \code{\link[stats]{chisq.test}}.
#' 
#' @details Computes confidence intervals for the proportion of the total in each level found in \code{$observed} of the \code{chi} object.  The confidence intervals are computed by treating each level as if it is the \dQuote{success} in a binomial confidence interval calculation (using \code{\link[FSA]{binCI}} from \pkg{FSA}).  One of three methods for computing each confidence interval can be used and is declared in the \code{type=} argument.  The three methods are described in detail for \code{\link[FSA]{binCI}}.
#' 
#' It should be noted that this is NOT the ideal method for computing confidence intervals for multinomial probabilities.  This area appears to receive a great deal of discussion, but two methoeds that appear to be generally accepted are due to Sison and Ganz (1995) when the number of cells (k) is \dQuote{large} (i.e., greater than 10) and Goodman (1965) when k is small (see May and Johnson 2000).  I have been unable to locate R code for the method of Sison and Ganz (1995) in any existing package, nor could I convert the SAS code provided by May and Johnson (2000) to R code.  As of July, 2011 there were at least two queries on R-help for the Sison and Ganz (1995) code with no replies.
#' 
#' This function was developed largely for use by my Introductory Statistics students in order to allow some post hoc discussion regarding significant results in goodness-of-fit tests.  This function was not developed for research-grade analyses.
#' 
#' @param chi An object from \code{\link[stats]{chisq.test}} representing a goodness-of-fit test
#' @param conf.level A number indicating the level of confidence to use for constructing confidence intervals (default is \code{0.95})
#' @param type A string that identifies the type of method to use for the calculations (see details)
#' @param digits A number representing the number of digits to which to print the results
#' 
#' @return A kx4 matrix, where k is the number of levels of the categorical variable, containing the observed proportion, the lower and upper confidence interval bounds for the population proportion, and the expected proportion used in the chi-square test.
#' 
#' 
#' @note This function is the Goodman method modified from code provided by Paul Rabie (then at the University  of Minnesota).
#' 
#' @seealso \code{\link[FSA]{binCI}} in \pkg{FSA}.
#' 
#' @references Glaz, J. and C.P. Sison.  1999.  Simultaneous confidence intervals for multinomial proportions.  Journal of Statistical Planning and Inference 82:251-262.
#' 
#' Goodman, L.A.  1965.  On simultaneous confidence intervals for multinomial proportions. Technometrics 7:247-254.
#' 
#' May, W.L. and W.D. Johnson.  2000.  Constructing two-sided simultaneous confidence intervals for multinomial proportions for small counts in a large number of cells.  Journal of Statistical Software 5(6).  Paper and code available at \url{http://www.jstatsoft.org/v05/i06}.
#' 
#' Sison, C.P and J. Glaz.  1995. Simultaneous confidence intervals and sample size determination for multinomial proportions.  Journal of the American Statistical Association, 90:366-369.  Paper available at \url{http://tx.liberal.ntu.edu.tw/~purplewoo/Literature/!Methodology/!Distribution_SampleSize/SimultConfidIntervJASA.pdf}
#' 
#' Wang, H.  2008.  Exact confidence coefficients of simultaneous confidence intervals for multinomial proportions.  Journal of Multivariate Analysis 99:896-911.
#' 
#' @keywords htest
#' 
#' @examples
#' \dontrun{
#' # example from chisq.test()
#' x <- c(A = 20, B = 15, C = 25)
#' ( x.chi <- chisq.test(x) )
#' gofCI(x.chi)
#' # note that all CIs contain expected proportions
#' 
#' # Another example from chisq.test()
#' x <- c(89,37,30,28,2)
#' p <- c(40,20,20,15,5)
#' ( x.chi <- chisq.test(x,p=p,rescale.p=TRUE) )
#' gofCI(x.chi)
#' # note that last CIs does not contain the expected proportion
#' 
#' # Same as above but using the Goodman method
#' gofCI(x.chi,type="goodman")
#' 
#' # A chi-square (not goodness-of-fit) test from chisq.test()
#' #M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
#' #dimnames(M) <- list(gender=c("M","F"), party=c("Democrat","Independent", "Republican"))
#' #( Xsq <- chisq.test(M) )
#' #try( gofCI(Xsq) )
#' # Gives an error as gofCI only works for goodness-of-fit results
#' 
#' # An example with only two levels (i.e., binomial situation)
#' obs <- c(A=56,B=34)
#' ( chi1 <- chisq.test(obs) )
#' gofCI(chi1)
#' 
#' # CI results should be the same as binom.test if type="exact" is used
#' gofCI(chi1,type="exact")
#' binom.test(obs)
#' 
#' # CI results should be the same as prop.test if type="wilson" (default)
#' #   and continuity correction is turned off
#' gofCI(chi1)
#' prop.test(obs[1],sum(obs),correct=FALSE)
#' 
#' # CI results should be the same as from normal theory if type="asymptotic"
#' gofCI(chi1,type="asymptotic")
#' p <- obs/sum(obs) 
#' se.p <- sqrt(p[1]*p[2]/sum(obs))
#' rbind(p[1]+c(1,-1)*qnorm(0.025)*se.p,p[2]+c(1,-1)*qnorm(0.025)*se.p)
#' }
#' 
#' @export
gofCI <- function(chi,conf.level=0.95,
                  type=c("wilson","exact","asymptotic","goodman"),
                  digits=getOption("digits")) {
  if (class(chi)!="htest") stop("'chi' argument must be the result of chisq.test()",call.=FALSE)
  if (chi$method != "Chi-squared test for given probabilities") {
    stop("'chi' argument must be the result of a goodness-of-fit test using chisq.test()",call.=FALSE) 
  }
  type <- match.arg(type)
  obs <- chi$observed
  lvls <- length(obs)
  if (type!="goodman") {
    # Ogle's ad hoc brute-force methods
    cis <- matrix(NA,nrow=lvls,ncol=2)
    # get CIs
    for (i in 1:lvls) cis[i,] <- FSA::binCI(obs[i],sum(obs),conf.level,type)
    res <- cbind(prop.table(obs),cis,chi$expected/sum(chi$expected))
  } else {
    # Goodman's method, modified from code by by Paul Rabie
    # Generally better if lvls<10
    A <- stats::qchisq((1-(1-conf.level)/lvls),1)
    N <- sum(obs)
    lci <- (A+2*obs-(A*(A+4*obs*(N-obs)/N))^0.5)/(2*(N+A))
    uci <- (A+2*obs+(A*(A+4*obs*(N-obs)/N))^0.5)/(2*(N+A))
    res <- cbind(prop.table(obs),lci,uci,chi$expected/sum(chi$expected))
  }
  colnames(res) <- c("p.obs","p.LCI","p.UCI","p.exp")
  rownames(res) <- names(obs)
  round(res,digits)
}
