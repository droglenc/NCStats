#' @title Puts Arbitrary Margins on Two-way Tables or Arrays
#' 
#' @description Used to put marginal values on the two-way table or array.  For tables or arrays with more dimensions see \code{addmargins()}.  This version is designed for use in introductory classes where the \code{margin=} argument has been modified to be more intuitive to students.  Specifically, This function is a pass-through to \code{addmargins()} with the exception that \code{margin=1} adds a marginal value at the end of the rows and \code{margin=2} adds a marginal value at the end of the columns.  The numeric values in \code{margin=} are opposite of what is used in \code{addmargins()}.
#' 
#' @param A table or array
#' @param margin A numeric value over which to form margins (see details)
#' @param \dots Additional arguments to be sent to \code{\link[stats]{addmargins}}
#' 
#' @return A table or array with the same number of dimensions as \code{A}, but with extra levels of the dimensions mentioned in \code{margin}.
#' 
#' @note This is primarily a wrapper function for \code{\link[stats]{addmargins}} in \pkg{stats}.
#' 
#' @seealso \code{\link{table}}, \code{\link[stats]{addmargins}}, and \code{\link{percTable}} in \pkg{NCStats}.
#' 
#' @keywords manip array
#' 
#' @examples
#' Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
#' ( A1 <- table(Aye) )
#' Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
#' ( A2 <- table(Aye, Bee) )
#' 
#' ## 1-D table
#' # Add sums to margin
#' addMargins(A1)
#' # Add sums to margin of proportions table
#' addMargins(prop.table(A1))
#' 
#' ## 2-D table
#' # Add sums to all margins
#' addMargins(A2)
#' # Add sums to end of rows (row totals)
#' addMargins(A2,margin=1)
#' # Add sums to end of columns (column totals)
#' addMargins(A2,margin=2)
#' # Add sums to end of a row proportions table
#' A2.row <- prop.table(A2,margin=1)
#' addMargins(A2.row,margin=1)
#' # Add sums to end of a column proportions table
#' A2.col <- prop.table(A2,margin=2)
#' addMargins(A2.col,margin=2)
#' # Add sums to all margins of proportions table
#' A2.tbl <- prop.table(A2)
#' addMargins(A2.tbl)
#' 
#' @export
addMargins <- function(A,margin=seq_along(dim(A)),...) {
  if (length(dim(A))>2) stop("addMargins only works with one or two dimensional tables.\n  See addmargins() for more complex tables.\n",call.=FALSE)
  if (length(dim(A))==2 & length(margin)==1) ifelse(margin==1,margin <- 2,margin <- 1)
  stats::addmargins(A,margin,...)
}



#' @title Constructs confidence intervals assuming normal distribution.
#' 
#' @description Returns the confidence interval endpoints given an estimate, SE, and df assuming that the estimate follows a normal distribution so that the t-distribution can be used when constructing the CI.
#' 
#' @param est A value that estimates a parameter (i.e., a statistic)
#' @param SE The standard error of the estimate
#' @param obsdf The degrees-of-freedom
#' @param conf.level The level of confidence as a decimal
#' 
#' @return Returns a matrix containing the lower and upper values of the confidence interval.
#' 
#' @seealso \code{\link[FSA]{confint.nlsBoot}} in \pkg{FSA}.
#' 
#' @keywords htest
#' 
#' @examples
#' 
#' ci.t(2.96,0.32,14)
#' 
#' @export
ci.t <- function(est,SE,obsdf,conf.level=0.95) {
  hw <- stats::qt(0.5+conf.level/2,obsdf)*SE
  res <- cbind(est-hw,est+hw)
  colnames(res) <- c(paste(paste0(round(100*conf.level,1),"%"),c("LCI","UCI")))
  res
}



#' @title Converts results in a contingency table into the original raw data.
#' 
#' @description Converts results in a contingency table into the original raw data.
#' 
#' @param x A matrix containing the contingency table.
#' @param var.names A vector of strings used to name the variables in the new data frame.
#' @param \dots Additional arguments for the type.convert function.
#' 
#' @return Returns a data frame of the expanded raw data from the contingency table.
#' 
#' @note This function is largely an implementation of the commands detailed in Mark Schwartz's R-help e-mail of 17Oct06 and 5Oct07.
#' 
#' @seealso \code{as.data.frame.table}, \code{type.convert}, and \code{expandTable} in \pkg{epitools}.
#' 
#' @keywords manip
#' 
#' @examples
#' ## Simple example
#' d <- matrix(c(3,2,1,1,2,3,3,2,1),nrow=3,byrow=TRUE)
#' colnames(d) <- c(2,4,6)
#' rownames(d) <- c(1,2,3)
#' d
#' rawd <- expandTable(d,c("RowVar","ColVar"))
#' rawd
#' with(rawd,table(RowVar,ColVar))
#' 
#' ## Example with zeroes, that is not square, and factor label names
#' f <- matrix(c(0,2,1,1,0,3,1,2,0,0),nrow=2,byrow=TRUE)
#' colnames(f) <- c("c1","c2","c3","c4","c5")
#' rownames(f) <- c("r1","r2")
#' f
#' rawf <- expandTable(f,c("RowVar","ColVar"))
#' rawf
#' with(rawf,table(RowVar,ColVar))
#' 
#' @export
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
      df[[i]] <- utils::type.convert(as.character(df[[i]]),...)
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




#' @title Labels a list of points on a two-dimensional plot.
#' 
#' @description Labels a list of points on a two-dimensional plot.
#' 
#' @details A two dimensional plot must be active and the \code{x} and \code{y} vectors must correspond to the x- and y-axes of the plot or \code{x} must correspond to the formula used to produce the plot.
#' 
#' @aliases highlight highlight.default highlight.formula
#' 
#' @param x The vector of x coordinates or a formula of the form \code{y~x}.
#' @param y The vector of y coordinates.
#' @param data The data frame from which the formula should be evaluated.
#' @param lbls The name of the variable in \code{data} or the parent environment that contains the labels for the points.  If left as \code{NULL} then the points will be labelled with the row number in \code{pts}.
#' @param pts The vector of row numbers corresponding to the points to highlight.  If left as \code{NULL} then every point will be labelled.
#' @param col A numeric or string indicating the color to use when labeling the points.
#' @param cex A numeric characther expansion number for the point labels.
#' @param pos A numeric indicating where to place the labels (see \code{text}.  If left as \code{NULL} then the labels will be placed to minimize label overlap as determined with \code{thigmophobe()} from \pkg{plotrix}.
#' @param \dots Other arguments to be passed to \code{text()}.
#' 
#' @return None, but an active graphic is modified.
#' 
#' @keywords hplot
#' 
#' @examples
#' df <- data.frame(x=runif(10),y=runif(10),grp=factor(rep(c("Yes","No"),each=5)))
#' plot(y~x,data=df)
#' 
#' # highlight the first and second points with default positioning using the formula notation
#' highlight(y~x,data=df,lbls=grp,pts=c(1,2))
#' # highlight the third and fourth points with user-chosen positioning
#' highlight(y~x,data=df,lbls=grp,pts=c(3,4),pos=c(1,2),col="blue")
#' # highlight the fifth and sixth points illustrating lack of data= argument
#' with(df,highlight(y~x,lbls=grp,pts=c(5,6),col="orange"))
#' # highlight the seventh and eighth points illustrating no use of formula
#' with(df,highlight(x,y,lbls=grp,pts=c(7,8),col="yellow"))
#' # highlight the ninth and tenth points with numeric labeling
#' highlight(y~x,data=df,pts=c(9,10),col="green")
#' 
#' # label all points with their row numbers
#' plot(y~x,data=df)
#' highlight(y~x,data=df)
#' 
#' @rdname highlight
#' @export
highlight <- function (x, ...) {
  UseMethod("highlight") 
}

#' @rdname highlight
#' @export
highlight.default <- function(x,y,lbls=NULL,pts=NULL,col="red",cex=1.25,pos=NULL,...) {
  if (is.null(y)) stop("Y-axis variable is missing",call.=FALSE)
  if (is.null(pts)) pts <- 1:length(x)
  if (is.null(lbls)) lbls <- as.character(1:length(x))
  if (is.null(pos)) pos <- plotrix::thigmophobe(x,y)
  graphics::text(x[pts],y[pts],lbls[pts],col=col,cex=cex,xpd=NA,pos=pos,...)
}

#' @rdname highlight
#' @export
highlight.formula <- function(x,data=NULL,lbls=NULL,pts=NULL,col="red",cex=1.25,pos=NULL,...) {
  mf <- stats::model.frame(x,data)
  x <- mf[,2]
  y <- mf[,1]
  if (!is.null(data)) lbls <- eval(substitute(lbls), data, environment(stats::formula))
  else lbls <- eval(substitute(lbls), sys.parent(), environment(stats::formula))
  highlight.default(x,y,lbls,pts,col,cex,pos,...)
}




#' @title Identify points in a plot using a formula.
#' 
#' @description Reads the position of the graphics pointer when the (first) mouse button is pressed, searches the coordinates given in x and y for the point closest to the pointer, and return the index if this point is close enough to the pointer.
#' 
#' @details This function is meant to make it easier to call \code{\link[graphics]{identify}} after \code{plot} has been called using a formula and the \code{data=} argument.
#' 
#' A two dimensional plot must be active and the vectors in \code{x} and data frame  in \code{data} must correspond to the x- and y-axes and the data of the plot.
#' 
#' @param x A formula of the form \code{y~x}.
#' @param data The data frame from which the formula should be evaluated.
#' @param \dots Other arguments to be passed to \code{\link[graphics]{identify}}.
#' 
#' @return See \code{identify}
#' 
#' @seealso \code{identify}
#' 
#' @keywords hplot
#' 
#' @examples
#' ## Not run by examples().  Copy and try in an interactive R session
#' \dontrun{
#' df <- data.frame(x=runif(10),y=runif(10),grp=factor(rep(c("Yes","No"),each=5)))
#' plot(y~x,data=df)
#' identify(y~x,data=df)
#' }
#' 
#' @rdname identify.formula
#' @export
identify.formula <- function(x,data=NULL,...) {
  mf <- stats::model.frame(x,data)
  x <- mf[,2]
  y <- mf[,1]
  graphics::identify(x,y,,...)
}




#' @title Multiple Simulations from Normal Distributions
#' 
#' @description Random generation from multiple normal distributions with potentially different means and standard deviations.
#' 
#' @details All of \code{n}, \code{mean}, \code{sd}, and \code{grp.labels} must be of the same length.
#' 
#' If \code{digits} is non-null and \code{exact=TRUE} the resulting quantitative data will only be approximately exact (due to the rounding).
#' 
#' @param n vector of number of observations.
#' @param mean vector of means.
#' @param sd vector of standard deviations.
#' @param exact a logical that indicates whether the resulting vector of random numbers will have the exact mean and standard deviation supplied in \code{mean} and \code{sd}.
#' @param grp.labels Labels for the levels representing the different groups.
#' @param var.labels Labels for or names for the columns of the resulting data.frame.
#' @param digits A number of digits to which the numeric data should be rounded.
#' 
#' @return A data.frame with two columns is returned.  The first columns is the random normal deviates and the second column are the group levels.
#' 
#' @note This function can be used to generate \dQuote{realistic} data when one knows the sample size, mean, and standard deviation for several groups and it can be assumed that the data in each group follows a normal distribution.  Thus, this function can be used to generate \dQuote{actual} data for, for example, one-way and two-way ANOVA from summaries of group sample sizes, means, and standard deviations.  Note that standard deviations can often be estimated by \dQuote{back-calculating} from given standard errors or confidence intervals.
#' 
#' @seealso \code{\link[stats]{rnorm}}.
#' 
#' @keywords distribution
#' 
#' @examples
#' # using default names
#' rand.data <- mrnorm(n=c(10,15,20),mean=c(10,15,15),sd=c(3,4,5))
#' # using custom names
#' rand.data1 <- mrnorm(n=c(10,15),mean=c(10,15),sd=c(3,4),
#'   grp.labels=c("First","Second"),var.labels=c("Y","X"))
#' 
#' if (require(FSA)) { 
#' Summarize(measure~group,data=rand.data)
#' Summarize(Y~X,data=rand.data1)
#' }
#' 
#' @export
mrnorm <- function(n,mean,sd,exact=TRUE,grp.labels=LETTERS[1:length(n)],
                   var.labels=c("measure","group"),digits=NULL) {
  if (any(diff(c(length(n),length(mean),length(sd),length(grp.labels)))!=0)) stop("One of n, mean, sd, grp.labels is of different length.",call.=FALSE)
  d <- grps <- NULL
  for (i in 1:length(n))  {
    d1 <- stats::rnorm(n[i],mean[i],sd[i])
    if (exact) {
      z <- (d1-mean(d1))/stats::sd(d1)
      d1 <- sd[i]*z+mean[i]
    }
    d <- c(d,d1)
    grps <- c(grps,rep(grp.labels[i],each=n[i]))
  }
  if (!is.null(digits)) d <- round(d,digits)
  ans <- data.frame(d,grps)
  colnames(ans) <- var.labels
  ans
}




#' @title Extract a simple random sample from a data.frame.
#' 
#' @description Extract a simple random sample from a data.frame, allowing user to choose particular variables.
#' 
#' @param df The data frame to be sampled from.
#' @param n Number of individuals (i.e., rows) to sample.
#' @param replace A logical indicating whether sampling with replacement should be used or not (default).
#' @param vars A vector of strings indicating which variables to include in the returned data.frame.
#' 
#' @return Returns a data.frame of rows sampled from \code{df}.  Optionally, only includes the columns listed in \code{vars}.
#' 
#' @seealso \code{\link{sample}}.
#' 
#' @keywords manip
#' 
#' @examples
#' data(ABDLakes)
#' 
#' # sample all variables without replacement
#' ( res1 <- srsdf(ABDLakes,50) )
#' 
#' # sample two variables without replacement
#' ( res2 <- srsdf(ABDLakes,50,vars=c("name","area")) )
#' 
#' # sample two variables with replacement
#' ( res3 <- srsdf(ABDLakes,50,replace=TRUE,vars=c("name","area")) )
#' 
#' @export
srsdf <- function(df,n,replace=FALSE,vars=NULL) {
  rows <- sample(1:nrow(df),n,replace=replace)
  if (is.null(vars)) df[rows,]
  else {
    lnames <- vars %in% names(df)
    if (all(lnames)) df[rows,vars]
    else stop(paste(paste(vars[lnames],collapse=", "),"is (are) not name(s) found in the data frame."),call.=FALSE)
  }
}




#' @title Shows a random selection of rows from a data frame or matrix.
#'
#' @description Shows a random selection of rows from a data frame or matrix.
#'
#' @param x A data frame or matrix.
#' @param which A numeric or string vector that contains the column numbers or names to display.  Defaults to showing all columns.
#' @param n A single numeric that indicates the number of rows to display.
#'
#' @return No value is returned but a random (but sorted) selection of rows from the data frame is displayed. 
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @note If \code{n} is larger than the number of rows in \code{x} then \code{x} is displayed without randomizing the rows.
#'
#' @keywords manip
#'
#' @examples
#'data(iris)
#'view(iris)
#'view(iris,10)
#'view(iris,which=c("Sepal.Length","Sepal.Width","Species"))
#'view(iris,which=grep("Sepal",names(iris)))
#'
#'## Make a matrix for demonstration purposes only
#'miris <- as.matrix(iris[1:4,])
#'view(miris)
#'view(miris,10)
#'view(miris,10,which=2:4)
#'
#' @export
view <- function(x,n=6L,which=NULL) {
  if (!(is.matrix(x) | is.data.frame(x))) stop("'x' must be a matrix or data.frame.",call.=FALSE)
  stopifnot(length(n) == 1L)
  N <- nrow(x)
  n <- ifelse(n<0L,max(N+n,0L),min(n,N))
  if (is.null(which)) {
    if (is.matrix(x)) x[sort(sample(1:N,n)),]
    else x[sort(sample(1:N,n)),names(x)]
  } else x[sort(sample(1:N,n)),which]
}




#' @title Express table entries as percentage of marginal table.
#' 
#' @description Same as \code{\link{prop.table}} except that it returns percentages rather than proportions.
#' 
#' @param x A frequency table likely constructed with \code{\link{table}} or \code{\link{xtabs}}.
#' @param margin A numeric representing an index, or vector of indices, to generate the margin for -- \code{margin=1} computes row percentages, \code{margin=2} computes column percentages, and \code{margin=NULL} (default) produces table percentages.
#' @param digits A numeric indicating the number of decimals to round the percentages to.
#' @param addMargins A logical indicating whether marginal totals should be appended to the table or not.  If \code{addMargins=TRUE} then the appended marginal totals will correspond to which margin is chosen with \code{margin=} (as in \code{\link{addMargins}}).
#' 
#' @return Same type as \code{x} except with percentages of a margin rather than frequencies.
#' 
#' @keywords manip
#' 
#' @examples
#' Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
#' Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
#' ( A <- table(Aye, Bee) )
#' 
#' ## what prop.table() would look like
#' prop.table(A,margin=1)
#' 
#' ## modified for percTable
#' # row percentages
#' percTable(A,margin=1)
#' percTable(A,margin=1,digits=1)
#' # column percentages
#' percTable(A,margin=2)
#' # table percentages
#' percTable(A)
#' 
#' ## No marginal totals
#' percTable(A,addMargins=FALSE)
#' 
#' @export
percTable <- function(x,margin=NULL,digits=getOption("digits"),addMargins=TRUE) {
  res <- round(100*prop.table(x,margin=margin),digits)
  if (addMargins) ifelse(is.null(margin),res <- addMargins(res),res <- addMargins(res,margin=margin))
  res
}
