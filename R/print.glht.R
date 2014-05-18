#'Modificaton of print.summary.glht() and print.confint.glth() to streamline output.
#' 
#'Modifies the print.summary.glht() and confint.summary.glht() functions so that the output is less verbose.  For example, several labels and some intermediate calculations will not be printed unless asked for.  In addition, several  \sQuote{extra} blank lines were removed.
#'
#'@aliases print.summary.glht print.confint.glht
#'
#'@param x An object of class \code{summary.glht} or \code{confint.glth} to be printed.
#'@param digits A single numeric that indicates the number of digits to use.
#'@param justResults A single logical that indicates whether just the most pertinent numerical results are printed (\code{=TRUE}; default) or all information as in the default functions from \pkg{multcomp}.
#'@param signif.stars A logical that if \code{=TRUE} then \sQuote{significance stars} will be printed for each coefficient.
#'@param \dots Not implemented.
#'
#'@return Invisibly returns the sent \code{x}.
#'
#'@examples
#'# from glht() in multcomp
#'require(multcomp)
#'amod <- aov(breaks ~ tension, data = warpbreaks)
#'mc1 <- glht(amod, linfct = mcp(tension = "Tukey"))
#' 
#'# new printing versions
#'summary(mc1)
#'confint(mc1)
#' 
#'# nearly the same as the original functions
#'print(summary(mc1),justResults=FALSE)
#'print(confint(mc1),justResults=FALSE)
#'
#'@rdname print.glht
#'@export
print.summary.glht <- function (x,digits=max(3,getOption("digits")-3),signif.stars=getOption("show.signif.stars"),justResults=TRUE,...)  {
  if (!justResults) {
    cat("Simultaneous Tests for General Linear Hypotheses\n")
    if (!is.null(x$type)) cat("Multiple Comparisons of Means:", x$type, "Contrasts\n")
    call <- if (isS4(x$model)) 
      x$model@call
    else x$model$call
    if (!is.null(call)) {
      cat("Fit: ")
      print(call)
      cat("\n")
    }
  }
  pq <- x$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df == 0, "z", "t"), ")", sep = ""), 
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), 
                  two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = "")) 
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df == 0, "z value", "t value"), pname)
  type <- pq$type
  if (!is.null(error) && error > .Machine$double.eps) {
    sig <- which.min(abs(1/error - (10^(1:10))))
    sig <- 1/(10^sig)
  }
  else { sig <- .Machine$double.eps }
  if (!justResults) cat("Linear Hypotheses:\n")
  alt <- switch(x$alternative, two.sided = "==", less = ">=", greater = "<=")
  rownames(mtests) <- paste(rownames(mtests), alt, x$rhs)
  printCoefmat(mtests, digits = digits, has.Pvalue = TRUE, P.values = TRUE, eps.Pvalue = sig)
  if (!justResults) {
    switch(type, univariate = cat("(Univariate p values reported)"), 
           `single-step` = cat("(Adjusted p values reported -- single-step method)"), 
           Shaffer = cat("(Adjusted p values reported -- Shaffer method)"), 
           Westfall = cat("(Adjusted p values reported -- Westfall method)"), 
           cat("(Adjusted p values reported --", type, "method)"))
  }
  cat("\n")
  invisible(x)
}

#'@rdname print.glht
#'@export
print.confint.glht <- function(x,digits=max(3,getOption("digits")-3),justResults=TRUE, ...) {
  xtmp <- x
    if (!justResults) {
    cat("Simultaneous Confidence Intervals\n")
    if (!is.null(x$type)) cat("Multiple Comparisons of Means:", x$type, "Contrasts\n")
    level <- attr(x$confint, "conf.level")
    attr(x$confint, "conf.level") <- NULL
    cat("Fit: ")
    if (isS4(x$model)) { print(x$model@call) }
    else { print(x$model$call)   }
    cat("\n")
    error <- attr(x$confint, "error")
    if (!is.null(error) && error > .Machine$double.eps) digits <- min(digits, which.min(abs(1/error - (10^(1:10)))))
    cat("Quantile =", round(attr(x$confint, "calpha"), digits))
    cat("\n")
    if (attr(x, "type") == "adjusted") { cat(100*level,"% family-wise confidence level\n\n\n",sep="") }
    else { cat(100*level,"% confidence level\n\n\n",sep="") }
  }
  if (!justResults) cat("Linear Hypotheses:\n")
  alt <- switch(x$alternative, two.sided = "==", less = ">=", greater = "<=")
  rownames(x$confint) <- paste(rownames(x$confint), alt, x$rhs)
  print(format(x$confint, nsmall = digits, digits = digits), quote = FALSE)
  cat("\n")
  invisible(xtmp)
}