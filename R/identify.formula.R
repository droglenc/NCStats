#' Identify points in a plot using a formula.
#' 
#' identify reads the position of the graphics pointer when the (first) mouse button is pressed. It then searches the coordinates given in x and y for the point closest to the pointer. If this point is close enough to the pointer, its index will be returned as part of the value of the call.
#' 
#' This function is meant to make it easier to call \code{\link[graphics]{identify}} after \code{plot} has been called using a formula and the \code{data=} argument.
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
