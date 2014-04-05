#'Tests for significant differences among all pairs of populations in a
#'chi-square test.
#'
#'Tests for significant differences among all pairs of populations in a
#'chi-square test.
#'
#'Post-hoc tests for which pairs of populations differ following a significant
#'chi-square test can be constructed by performing all chi-square tests for all
#'pairs of populations and then adjusting the resulting p-values for inflation
#'due to multiple comparisons.  The adjusted p-values can be computed with a
#'wide variety of methods -- fdr, BH, BY, bonferroni, holm, hochberg, and
#'hommel.  This function basically works as a wrapper function that sends the
#'unadjusted \dQuote{raw} p-values from each pair-wise chi-square test to the
#'\code{p.adjust} function in the base R program.  The \code{p.adjust} function
#'should be consulted for further description of the methods used.
#'
#'@param chi A \code{chisq.test} object.
#'@param popsInRows A logical indicating whether the populations form the rows
#'(default; \code{=TRUE}) of the table or not (\code{=FALSE}).
#'@param control A string indicating the method of control to use.  See
#'details.
#'@param digits A numeric that controls the number of digits to print.
#'@param \dots Other arguments sent to \code{print}.
#'@return A data.frame with a description of the pairwise comparisons, the raw
#'p-values, and the adjusted p-values.
#'@seealso \code{chisq.test} and \code{p.adjust}.
#'@keywords htest
#'@examples
#'# Makes a table of observations -- similar to first example in chisq.test
#'M <- as.table(rbind(c(76, 32, 46), c(48,23,47), c(45,34,78)))
#'dimnames(M) <- list(sex=c("Male","Female","Juv"),loc=c("Lower","Middle","Upper"))
#'M
#'# Fits chi-square test and shows summary
#'( chi1 <- chisq.test(M) )
#'# Shows post-hoc pairwise comparisons using fdr method
#'chisqPostHoc(chi1)
#'
#'# Transpose the observed table to demonstrate use of popsInRows=FALSE
#'( chi2 <- chisq.test(t(M)) )
#'chisqPostHoc(chi2,popsInRows=FALSE)
#'
#'@export
chisqPostHoc <- function(chi,popsInRows=TRUE,control=c("fdr","BH","BY","bonferroni","holm","hochberg","hommel"),digits=4) {
  control <- match.arg(control)
  tbl <- chi$observed
  if (!popsInRows) tbl <- t(tbl)
  popsNames <- rownames(tbl)
  
  prs <- combn(1:nrow(tbl),2)
  tests <- ncol(prs)
  pvals <- numeric(tests)
  lbls <- character(tests)
  for (i in 1:tests) {
    pvals[i] <- chisq.test(tbl[prs[,i],])$p.value
    lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
  }
  adj.pvals <- p.adjust(pvals,method=control)
  cat("Adjusted p-values used the",control,"method.\n\n")
  data.frame(comparison=lbls,raw.p=round(pvals,digits),adj.p=round(adj.pvals,digits))
}
