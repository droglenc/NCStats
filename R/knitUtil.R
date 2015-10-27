#' @title Specific utilities for use in a knitr document.
#'
#' @description Specific utilities for pretty printing various items in a knitr document.
#'
#' @details
#'  \itemize{
#'    \item \code{kANOVA} is used to print \sQuote{pretty} ANOVA tables (i.e., some output removed (see below)).
#'    \item \code{kGLHT} is used to print \sQuote{pretty} multiple comparison tables (i.e., some output removed (see below)).
#'    \item \code{kREG} is used to print \sQuote{pretty} summary regression results (i.e., some output removed (see below)).
#'    \item \code{kHtest} is used to print \sQuote{pretty} hypothesis test (e.g., from \code{\link{t.test}} or \code{\link{chisq.test}}) tables (i.e., some output removed (see below)).
#'  }
#'
#' @aliases kANOVA kGLHT kREG kHtest
#' 
#' @param x An object saved from \code{lm} or \code{glht} or an object of class \code{htest} (e.g., saved from \code{t.test}).
#' @param digits Number of decimal places to round the values to.
#' @param type A string that indicates the type of glht result to extract.
#' @param show.alt A logical that indicates whether the line stating what the alternative hypothesis is should be printed (\code{TRUE}) or not (\code{FALSE}; default).
#' @param \dots Additional arguments for the original \code{Stangle} or \code{purl}.
#'
#' @return
#'  \itemize{
#'    \item \code{kANOVA} returns the results of \code{anova} but without the heading attribute. 
#'    \item \code{kGLHT} returns a matrix of just the hypothesis test or confidence interval results from a \code{glht} object (i.e., all messages that are usually printed are stripped away). 
#'    \item \code{kREG} returns the results of \code{summary} but without the call and the information about the residuals. 
#'    \item \code{kHtest} returns the same as \code{stats:::print.htest} except that the name of the test, the data used, and, optionally, the descriptor of the alternative hypothesis are not printed. 
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link[multcomp]{glht}} in \pkg{multcomp} for related functionality.
#'
#' @keywords hplot models manip
#'
#' @examples
#' # None yet
#'
#' @rdname knitUtil
#' @export
kANOVA <- function(x) {
  x <- stats::anova(x)
  attr(x,"heading") <- NULL
  x
}

#' @rdname knitUtil
#' @export
kGLHT <- function(x,type=c("hypothesis","confidence")) {
  type <- match.arg(type)
  if (type=="hypothesis") {
    int <- summary(x)$test
    res <- cbind(int$coefficients,int$sigma,int$tstat,int$pvalues)
    colnames(res) <- c("Estimate","Std. Error","t value","p value")
    rownames(res) <- paste (rownames(res),"= 0")
  } else res <- confint(x)$confint[,1:3]
  res
}

#' @rdname knitUtil
#' @export
kREG <- function(x,digits=max(3,getOption("digits")-3)) {
  x <- summary(x)
  rdf <- x$df[2L]
  coefs <- x$coefficients
  if (!is.null(aliased <- x$aliased) && any(aliased)) {
    cn <- names(aliased)
    coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
    coefs[!aliased, ] <- x$coefficients
  }
  stats::printCoefmat(coefs, digits = digits, na.print = "NA")
  cat("---")
  cat("\nResidual standard error:", format(signif(x$sigma,digits)), "on", rdf, "degrees of freedom\n")
  if (nzchar(mess <- stats::naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared:", formatC(x$adj.r.squared,digits = digits), "\nF-statistic:",
        formatC(x$fstatistic[1],digits=digits), "on", x$fstatistic[2], "and", x$fstatistic[3], 
        "DF,  p-value:",base::format.pval(pf(x$fstatistic[1L],x$fstatistic[2L],x$fstatistic[3L],lower.tail=FALSE),digits=digits), "\n")
    }
  cat("\n")
}
                
#' @rdname knitUtil
#' @export
kHtest <- function(x,digits=4,show.alt=FALSE,...) {
  # the same as stats:::print.htest without the test name and listing the data
  outp <- character()
  if (show.alt & !is.null(x$alternative)) {
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
      if (length(x$null.value) == 1L) {
        alt.char <- switch(x$alternative,two.sided="not equal to",less="less than",greater="greater than")
        cat("true",names(x$null.value),"is",alt.char,x$null.value, "\n")
      }
      else {
        cat(x$alternative, "\nnull values:\n")
        print(x$null.value, ...)
      }
    }
    else cat(x$alternative, "\n")
  }
  if (!is.null(x$statistic)) outp <- c(outp,paste(names(x$statistic),"=",format(round(x$statistic,4))))
  if (!is.null(x$parameter)) outp <- c(outp,paste(names(x$parameter),"=",format(round(x$parameter,3))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value,digits=digits)
    outp <- c(outp,paste("p-value",if(substr(fp,1L,1L) == "<") fp else paste("=",fp)))
  }
  cat(strwrap(paste(outp,collapse=", ")),sep="\n")
  if (!is.null(x$conf.int)) {cat(format(100*attr(x$conf.int,"conf.level")),"percent confidence interval:\n",format(c(x$conf.int[1L],x$conf.int[2L])),"\n") }
  if (!is.null(x$estimate)) {
    cat("sample estimates:\n")
    print(x$estimate, ...)
  }
  cat("\n")
  invisible(x)
}
