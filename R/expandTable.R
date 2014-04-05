#'Converts results in a contingency table into the original raw data.
#'
#'Converts results in a contingency table into the original raw data.
#'
#'@param x A matrix containing the contingency table.
#'@param var.names A vector of strings used to name the variables in the new
#'data frame.
#'@param \dots Additional arguments for the type.convert function.
#'@return Returns a data frame of the expanded raw data from the contingency
#'table.
#'@note This function is largely an implementation of the commands detailed in
#'Mark Schwartz's R-help e-mail of 17Oct06 and 5Oct07.
#'@seealso \code{as.data.frame.table}, \code{type.convert}, and
#'\code{expandTable} in \pkg{epitools}.
#'@keywords manip
#'@examples
#'## Simple example
#'d <- matrix(c(3,2,1,1,2,3,3,2,1),nrow=3,byrow=TRUE)
#'colnames(d) <- c(2,4,6)
#'rownames(d) <- c(1,2,3)
#'d
#'rawd <- expandTable(d,c("RowVar","ColVar"))
#'rawd
#'with(rawd,table(RowVar,ColVar))
#'
#'## Example with zeroes, that is not square, and factor label names
#'f <- matrix(c(0,2,1,1,0,3,1,2,0,0),nrow=2,byrow=TRUE)
#'colnames(f) <- c("c1","c2","c3","c4","c5")
#'rownames(f) <- c("r1","r2")
#'f
#'rawf <- expandTable(f,c("RowVar","ColVar"))
#'rawf
#'with(rawf,table(RowVar,ColVar))
#'
#'@export
expandTable <- function(x,var.names=NULL,...) {
  Freq <- NULL  # attempting to halt "no visible bindings" warning on RCMD CHECK
  nr <- nrow(x)
  nc <- ncol(x)
  if (nr==1 | nc==1) {
    # if only one row then transpose to only one column
    if (nr==1) x <- t(x)
    df <- data.frame(rep(rownames(x),x))
    if (length(var.names)>1) stop ("Too many var.names given.",call.=FALSE)
    names(df) <- var.names
  } else {
    # converts table to a data.frame
    x <- as.data.frame.table(x)     
    # replicate each value in x by frequency                                                   
    df <- sapply(1:nrow(x),function(i) x[rep(i,each=x[i,"Freq"]),],simplify=FALSE)
    # results from above rbound and deleted Freq column     
    df <- subset(do.call("rbind",df),select=-Freq)                                     
    for (i in 1:ncol(df)) {
      df[[i]] <- type.convert(as.character(df[[i]]),...)
    }
    rownames(df) <- NULL
    if (!is.null(var.names)) {
      if (length(var.names) < 2) stop("Too few var.names given.",call.=FALSE)
      else if (length(var.names) > 2) stop ("Too many var.names given.",call.=FALSE)
        else names(df) <- var.names
    }
  }
  df
}
