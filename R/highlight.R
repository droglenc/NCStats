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
  if (iChk4Namespace("plotrix")) {
    if (is.null(pos)) pos <- plotrix::thigmophobe(x,y)
  }
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
