#'Express table entries as percentage of marginal table.
#'
#'Same as \code{prop.table()} except that it returns percentages rather than
#'proportions.
#'
#'@param x A frequency table likely constructed with table().
#'@param margin A numeric representing an index, or vector of indices, to
#'generate the margin for -- \code{margin=1} computes row percentages,
#'\code{margin=2} computes column percentages, and \code{margin=NULL} (default)
#'produces table percentages.
#'@param digits A numeric indicating the number of decimals to round the
#'percentages to..
#'@param addMargins A logical indicating whether marginal totals should be
#'appended to the table or not.  If \code{addMargins=TRUE} then the appended
#'marginal totals will correspond to which margin is chosen with
#'\code{margin=}.
#'@return Same type as \code{x} except with percentages of a margin rather than
#'frequencies.
#'@seealso \code{\link{prop.table}}, \code{\link{addmargins}}, and \code{addMargins} in \pkg{FSA}.
#'@keywords manip
#'@examples
#'Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
#'Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
#'( A <- table(Aye, Bee) )
#'
#'## what prop.table() would look like
#'prop.table(A,margin=1)
#'
#'## modified for percTable
#'# row percentages
#'percTable(A,margin=1)
#'percTable(A,margin=1,digits=1)
#'# column percentages
#'percTable(A,margin=2)
#'# table percentages
#'percTable(A)
#'
#'## No marginal totals
#'percTable(A,addMargins=FALSE)
#'
#'@export
percTable <- function(x,margin=NULL,digits=getOption("digits"),addMargins=TRUE) {
  res <- round(100*prop.table(x,margin=margin),digits)
  if (addMargins) ifelse(is.null(margin),res <- addMargins(res),res <- addMargins(res,margin=margin))
  res
}
