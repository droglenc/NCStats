#' @title Modificaton of print.summary.glht() and print.confint.glth() to streamline output.
#'  
#' @description Modifies the print.summary.glht() and confint.summary.glht() functions so that the output is less verbose.  For example, several labels and some intermediate calculations will not be printed unless asked for.  In addition, several  \sQuote{extra} blank lines were removed.
#' 
#' @aliases print.summary.glht print.confint.glht
#' 
#' @param x An object of class \code{\link[multcomp:methods]{summary.glht}} or \code{\link[multcomp:methods]{confint.glht}} to be printed
#' @param digits A single numeric that indicates the number of digits to use
#' @param justResults A single logical that indicates whether just the most pertinent numerical results are printed (\code{=TRUE}; default) or all information as in the default functions from \pkg{multcomp}
#' @param signif.stars A logical that if \code{=TRUE} then \sQuote{significance stars} will be printed for each coefficient
#' @param \dots Not implemented.
#' 
#' @return Invisibly returns the sent \code{x}.
#' 
#' @examples
#' # from glht() in multcomp
#' if (require(multcomp)) {
#'   amod <- aov(breaks ~ tension, data = warpbreaks)
#'   mc1 <- glht(amod, linfct = mcp(tension = "Tukey"))
#'   # new printing versions
#'   summary(mc1)
#'   confint(mc1)
#'   # nearly the same as the original functions
#'   print(summary(mc1),justResults=FALSE)
#'   print(confint(mc1),justResults=FALSE)
#' }
#' 
#' @rdname print.glht
#' @method print summary.glht
#' @export
print.summary.glht <- function (x,digits=max(3,getOption("digits")-3),
                                signif.stars=getOption("show.signif.stars"),
                                justResults=TRUE,...)  {
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
  stats::printCoefmat(mtests, digits = digits, has.Pvalue = TRUE, P.values = TRUE, eps.Pvalue = sig)
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

#' @rdname print.glht
#' @method print confint.glht
#' @export
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



#' @title Modificaton of print.htest() to streamline output.
#'  
#' @description Modifies the print.hstest() function so that the output is less verbose.  For example, several \sQuote{extra} blank lines are removed and the test name is placed on one line with the data.
#' 
#' @aliases print.htest 
#' 
#' @param x An object of class \code{lm} to be printed
#' @param digits A single numeric that indicates the number of digits to use
#' @param quote A single logical that indicates whether strings should be printed with surrounding quotes
#' @param \dots Not implemented
#' 
#' @return Invisibly returns the sent \code{x}.
#' 
#' @examples
#' ## data from t.test and chisq.test examples
#' t.test(1:10,y=c(7:20))
#' 
#' M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
#' chisq.test(M)
#' 
#' @rdname print.htest
#' @method print htest
#' @export
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




#' @title Modificaton of print.lm() to streamline output
#'  
#' @description Modifies the print.lm() function so that the output is less verbose.  For example, the call will not be re-printed unless asked for.  In addition, several \sQuote{extra} blank lines were removed.
#'  
#' @aliases print.lm
#' 
#' @param x An object of class \code{lm} or \code{summary.lm} to be printed.
#' @param digits A single numeric that indicates the number of digits to use.
#' @param show.call A single logical that indicates whether the call should be re-printed.
#' @param \dots Not implemented.
#' 
#' @return Invisibly returns the sent \code{x}.
#' 
#' @examples
#' ## from lm examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2,10,20, labels=c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' 
#' # the new print version of the lm object
#' lm.D9 <- lm(weight ~ group)
#' 
#' # the new print version of the summary.lm object
#' summary(lm.D9)
#' 
#' @method print lm
#' @export
print.lm <- function(x,digits=max(3,getOption("digits")-3),show.call=FALSE,...) {
  if (show.call) cat("Call:",paste(deparse(x$call),sep="\n",collapse="\n"),"\n\n",sep="")
  if (length(stats::coef(x))) {
    cat("Coefficients:\n")
    print.default(format(stats::coef(x),digits=digits),print.gap=2,quote=FALSE)
  }
  else cat("No coefficients\n")
  invisible(x)
}




#' @title Modificaton of print.summary.lm() to streamline output
#'  
#' @description Modifies the print.summary lm() function so that the output is less verbose.  For example, the call and information about the residuals will not be re-printed unless asked for.  In addition, several \sQuote{extra} blank lines were removed.
#'  
#' @aliases print.summary.lm print.summary.glm
#' 
#' @param x An object of class \code{summary.lm} to be printed
#' @param digits A single numeric that indicates the number of digits to use
#' @param show.call A single logical that indicates whether the call should be re-printed
#' @param show.resids A single logical that indicates whether the information about the residuals should be included when printing the object
#' @param symbolic.cor A logical that if \code{=TRUE} will print the correlations in a symbolic form rather than as numbers
#' @param signif.stars A logical that if \code{=TRUE} then \sQuote{significance stars} will be printed for each coefficient
#' @param \dots Not implemented
#' 
#' @return Invisibly returns the sent \code{x}.
#' 
#' @examples
#' ## from lm examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2,10,20, labels=c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' 
#' # the new print version of the lm object (see print.lm)
#' lm.D9 <- lm(weight ~ group)
#' # the new print version of the summary.lm object
#' summary(lm.D9)
#' 
#' ## from glm examples with new summary print
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#' glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
#' summary(glm.D93)
#' 
#' @rdname print.summary.lm
#' @method print summary.lm
#' @export
print.summary.lm <- function(x,digits=max(3,getOption("digits")-3),
                             symbolic.cor=x$symbolic.cor,
                             signif.stars=getOption("show.signif.stars"),
                             show.call=FALSE,show.resids=FALSE,...) {
  if (show.call) cat("Call: ",paste(deparse(x$call),sep="\n",collapse="\n"),"\n",sep="")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  if (show.resids) {
    cat("\n",if (!is.null(x$weights) && diff(range(x$weights))) "Weighted ","Residuals:\n",sep="")
    if (rdf > 5L) {
      nam <- c("Min", "1Q", "Median", "3Q", "Max")
      rq <- if (length(dim(resid)) == 2L) structure(apply(t(resid),1L,stats::quantile),
                                                    dimnames=list(nam,dimnames(resid)[[2L]]))
      else {
        zz <- zapsmall(stats::quantile(resid), digits + 1)
        structure(zz, names = nam)
      }
      print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
      print(resid, digits = digits, ...)
    }
    else {
      cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!\n")
    }
  }
  if (show.call | show.resids) cat("\n")
  if (length(x$aliased) == 0L) {
    cat("No Coefficients\n")
  }
  else {
    if (nsingular <- df[3L] - df[1L]) 
      cat("Coefficients: (", nsingular, " not defined because of singularities)\n",sep = "")
    else cat("Coefficients:\n")
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn,colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    stats::printCoefmat(coefs, digits = digits, signif.stars = signif.stars,na.print = "NA", ...)
  }
  cat("\nResidual standard error:", format(signif(x$sigma,digits)), "on", rdf, "degrees of freedom\n")
  if (nzchar(mess <- stats::naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared:", formatC(x$adj.r.squared,digits = digits),
        "\nF-statistic:", formatC(x$fstatistic[1L],digits = digits), "on",
        x$fstatistic[2L],"and",x$fstatistic[3L], "DF,  p-value:",
        format.pval(stats::pf(x$fstatistic[1L],x$fstatistic[2L],x$fstatistic[3L],
                              lower.tail=FALSE),digits = digits), "\n")
  }
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(stats::symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl,2),nsmall=2,digits=digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

#' @rdname print.summary.lm
#' @method print summary.glm
#' @export
print.summary.glm <- function(x,digits=max(3,getOption("digits")-3),symbolic.cor=x$symbolic.cor,
                              signif.stars = getOption("show.signif.stars"),show.call=FALSE,show.resids=FALSE,...) {
  if (show.call) cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
  if (show.resids) {
    cat("Deviance Residuals: \n")
    if (x$df.residual > 5) {
      x$deviance.resid <- stats::quantile(x$deviance.resid, na.rm = TRUE)
      names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", "Max")
    }
    xx <- zapsmall(x$deviance.resid, digits + 1)
    print.default(xx, digits = digits, na.print = "", print.gap = 2)
    cat("\n")
  }
  if (length(x$aliased) == 0L) cat("No Coefficients\n")
  else {
    df <- if ("df" %in% names(x)) x[["df"]]
    else NULL
    if (!is.null(df) && (nsingular <- df[3L] - df[1L])) 
      cat("Coefficients: (", nsingular, " not defined because of singularities)\n", sep = "")
    else cat("\nCoefficients:\n")
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4L, dimnames = list(cn,colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    stats::printCoefmat(coefs, digits = digits, signif.stars = signif.stars,na.print = "NA", ...)
  }
  cat("\n(Dispersion parameter for ", x$family$family, " family taken to be ", 
      format(x$dispersion), ")\n\n", apply(cbind(paste(format(c("Null","Residual"), 
                                                              justify = "right"), "deviance:"),
                                                 format(unlist(x[c("null.deviance","deviance")]), digits = max(5, digits + 1)), " on", 
                                                 format(unlist(x[c("df.null", "df.residual")])), " degrees of freedom\n"),1L,
                                           paste, collapse = " "), sep = "")
  if (nzchar(mess <- stats::naprint(x$na.action))) cat("  (", mess, ")\n",sep="")
  cat("AIC: ",format(x$aic,digits=max(4,digits+1)),"\n\n",
      "Number of Fisher Scoring iterations: ",x$iter,"\n",sep="")
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) print(stats::symnum(correl,abbr.colnames=NULL))
      else {
        correl <- format(round(correl, 2),nsmall=2,digits=digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1,-p,drop=FALSE],quote=FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}
