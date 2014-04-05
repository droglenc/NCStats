#'Modificaton of print.anova() to streamline output
#' 
#'Modifies the print.anova()) function so that the output is less verbose (the 
#'heading and several \sQuote{extra} blank lines were removed) and, if a single
#'anova object is sent, then a \sQuote{total} row will be added to the anova table.
#'
#'@aliases print.anova
#'@param x An object of class \code{anova} to be printed.
#'@param digits A single numeric that indicates the number of digits to use.
#'@param signif.stars A logical indicating whether significance starts should be printed.
#'@param totalSS A logical indicating whether the total df and SS row should be included or not.
#'@param rm.heading A logical indicating whether the heading in the anova object should
#'be included or not.
#'@param \dots Other arguments passed to \code{printCoefMat}.
#'@return Invisibly returns the sent, and possibly modified, \code{x}.
#'@examples
#'## example of one-way ANOVA results
#'lm1 <- lm(Sepal.Length~Species,data=iris)
#'# anova from 'stats' package
#'stats::print.anova(anova(lm1))
#'# anova as modified here
#'anova(lm1)
#'
#'## example of a simple linear regression
#'lm2 <- lm(Sepal.Length~Sepal.Width,data=iris)
#'anova(lm2)
#'
#'# example of indicator variable regression
#'lm3 <- lm(Sepal.Length~Sepal.Width*Species,data=iris)
#'anova(lm3)
#'
#'# example of multiple lm objects -- i.e., just passes through to stats::print.anova
#'lm4 <- lm(Sepal.Length~Sepal.Width+Species,data=iris)
#'anova(lm2,lm4,lm3)
#'
#'@export
#'@S3method print anova
print.anova <- function(x,digits=max(getOption("digits")-2,3),signif.stars=getOption("show.signif.stars"),
                        totalSS=TRUE,rm.heading=TRUE,...) {
  if (!any(grepl("Res.Df",colnames(x)))) {         # exclusion for when multiple lm objects are sent
    if (!any(grepl("Levene",attr(x,"heading")))) { # exclusion for levenes.test
      if (totalSS) {                               # add total SS row
        x <- rbind(x,c(sum(x$Df),sum(x[,"Sum Sq"]),NA,NA,NA))
        row.names(x)[dim(x)[1]] <- "Total"
      }
    }
  }
  if (rm.heading) attr(x,"heading") <- NULL        # remove heading
  stats::print.anova(x,digits=digits,signif.stars=signif.stars,...)
  invisible(x)
}