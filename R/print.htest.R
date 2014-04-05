#'Modificaton of print.htest() to streamline output.
#' 
#'Modifies the print.hstest() function so that the output is less verbose.  For
#'example, several \sQuote{extra} blank lines are removed and the test name is
#'placed on one line with the data.
#'
#'@aliases print.htest 
#'@param x An object of class \code{lm} to be printed.
#'@param digits A single numeric that indicates the number of digits to use.
#'@param quote A single logical that indicates whether strings should be printed
#'with surrounding quotes.
#'@param \dots Not implemented.
#'@return Invisibly returns the sent \code{x}.
#'@examples
#'## data from t.test and chisq.test examples
#'t.test(1:10,y=c(7:20))
#'
#'M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
#'chisq.test(M)
#'
#'@rdname print.htest
#'@method print htest
#'@S3method print htest
#'
print.htest <- function(x,digits=4,quote=TRUE,...) {
  cat(x$method,"with",x$data.name,"\n")
  out <- character()
  if (!is.null(x$statistic)) 
    out <- c(out, paste(names(x$statistic), "=", format(round(x$statistic,4))))
  if (!is.null(x$parameter)) 
    out <- c(out, paste(names(x$parameter), "=", format(round(x$parameter,3))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = digits)
    out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "<") fp else paste("=", fp)))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  if (!is.null(x$alternative)) {
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
      if (length(x$null.value) == 1L) {
        alt.char <- switch(x$alternative, two.sided = "not equal to", 
                           less = "less than", greater = "greater than")
        cat("true", names(x$null.value), "is", alt.char,x$null.value, "\n")
      }
      else {
        cat(x$alternative, "\nnull values:\n")
        print(x$null.value, ...)
      }
    }
    else cat(x$alternative, "\n")
  }
  if (!is.null(x$conf.int)) {
    cat(format(100*attr(x$conf.int,"conf.level")),"percent confidence interval:\n", 
        format(c(x$conf.int[1L], x$conf.int[2L])), "\n")
  }
  if (!is.null(x$estimate)) {
    cat("sample estimates:\n")
    print(x$estimate, ...)
  }
  cat("\n")
  invisible(x)
}