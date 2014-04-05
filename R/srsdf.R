#'Extract a simple random sample from a data.frame.
#'
#'Extract a simple random sample from a data.frame, allowing user to choose
#'particular variables.
#'
#'@param df The data frame to be sampled from.
#'@param n Number of individuals (i.e., rows) to sample.
#'@param replace A logical indicating whether sampling with replacement should
#'be used or not (default).
#'@param vars A vector of strings indicating which variables to include in the
#'returned data.frame.
#'@return Returns a data.frame of rows sampled from \code{df}.  Optionally,
#'only includes the columns listed in \code{vars}.
#'@seealso \code{\link{sample}}.
#'@keywords manip
#'@examples
#'data(ABDLakes)
#'
#'# sample all variables without replacement
#'( res1 <- srsdf(ABDLakes,50) )
#'
#'# sample two variables without replacement
#'( res2 <- srsdf(ABDLakes,50,vars=c("name","area")) )
#'
#'# sample two variables with replacement
#'( res3 <- srsdf(ABDLakes,50,replace=TRUE,vars=c("name","area")) )
#'
#'@export
srsdf <- function(df,n,replace=FALSE,vars=NULL) {
  rows <- sample(1:nrow(df),n,replace=replace)
  if (is.null(vars)) df[rows,]
    else {
      lnames <- vars %in% names(df)
      if (all(lnames)) df[rows,vars]
        else stop(paste(paste(vars[lnames],collapse=", "),"is (are) not name(s) found in the data frame."),call.=FALSE)
    }
}
