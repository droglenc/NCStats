#' Modificaton of print.summary.lm() to streamline output
#' 
#' Modifies the print.summary lm() function so that the output is less verbose.
#' For example, the call and information about the residuals will not be re-printed
#' unless asked for.  In addition, several \sQuote{extra} blank lines were removed.
#' 
#'@aliases print.summary.lm print.summary.glm
#'@param x An object of class \code{summary.lm} to be printed.
#'@param digits A single numeric that indicates the number of digits to use.
#'@param show.call A single logical that indicates whether the call should be re-printed.
#'@param show.resids A single logical that indicates whether the information about
#'the residuals should be included when printing the object.
#'@param symbolic.cor A logical that if \code{=TRUE} will print the correlations
#'in a symbolic form rather than as numbers.
#'@param signif.stars A logical that if \code{=TRUE} then \sQuote{significance stars}
#'will be printed for each coefficient.
#'@param \dots Not implemented.
#'@return Invisibly returns the sent \code{x}.
#'@examples
#'## from lm examples
#'ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#'trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#'group <- gl(2,10,20, labels=c("Ctl","Trt"))
#'weight <- c(ctl, trt)
#'
#'# the new print version of the lm object (see print.lm)
#'lm.D9 <- lm(weight ~ group)
#'# the new print version of the summary.lm object
#'summary(lm.D9)
#'
#'## from glm examples with new summary print
#'counts <- c(18,17,15,20,10,20,25,13,12)
#'outcome <- gl(3,1,9)
#'treatment <- gl(3,3)
#'d.AD <- data.frame(treatment, outcome, counts)
#'glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
#'summary(glm.D93)
#'
#'@method print summary.lm
#'@S3method print summary.lm
print.summary.lm <- function(x,digits=max(3,getOption("digits")-3),symbolic.cor=x$symbolic.cor,
                             signif.stars=getOption("show.signif.stars"),show.call=FALSE,show.resids=FALSE,...) {
  if (show.call) cat("Call: ",paste(deparse(x$call),sep="\n",collapse="\n"),"\n",sep="")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  if (show.resids) {
    cat("\n",if (!is.null(x$weights) && diff(range(x$weights))) "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
      nam <- c("Min", "1Q", "Median", "3Q", "Max")
      rq <- if (length(dim(resid)) == 2L) structure(apply(t(resid), 1L, quantile), dimnames = list(nam,dimnames(resid)[[2L]]))
      else {
        zz <- zapsmall(quantile(resid), digits + 1)
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
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,na.print = "NA", ...)
  }
  cat("\nResidual standard error:", format(signif(x$sigma,digits)), "on", rdf, "degrees of freedom\n")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared:", formatC(x$adj.r.squared,digits = digits),
        "\nF-statistic:", formatC(x$fstatistic[1L],digits = digits), "on", x$fstatistic[2L], "and", 
        x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L],x$fstatistic[2L],
                                                          x$fstatistic[3L], lower.tail = FALSE),digits = digits), "\n")
  }
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
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

#'@method print summary.glm
#'@S3method print summary.glm
print.summary.glm <- function(x,digits=max(3,getOption("digits")-3),symbolic.cor=x$symbolic.cor,
                              signif.stars = getOption("show.signif.stars"),show.call=FALSE,show.resids=FALSE,...) {
  if (show.call) cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
  if (show.resids) {
    cat("Deviance Residuals: \n")
    if (x$df.residual > 5) {
      x$deviance.resid <- quantile(x$deviance.resid, na.rm = TRUE)
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
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,na.print = "NA", ...)
  }
  cat("\n(Dispersion parameter for ", x$family$family, " family taken to be ", 
      format(x$dispersion), ")\n\n", apply(cbind(paste(format(c("Null","Residual"), 
      justify = "right"), "deviance:"), format(unlist(x[c("null.deviance","deviance")]), digits = max(5, digits + 1)), " on", 
      format(unlist(x[c("df.null", "df.residual")])), " degrees of freedom\n"),1L, paste, collapse = " "), sep = "")
  if (nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
  cat("AIC: ", format(x$aic, digits = max(4, digits + 1)),"\n\n", "Number of Fisher Scoring iterations: ", x$iter,"\n", sep = "")
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) print(symnum(correl, abbr.colnames = NULL))
      else {
        correl <- format(round(correl, 2), nsmall = 2, digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}