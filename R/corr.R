#' @title Compute correlation coefficients.
#'
#' @description Computes the correlation coefficients between \code{y} and \code{x} if these are vectors.  If \code{y} and \code{x} are matrices then the correlation between the columns of \code{y} and the columns of \code{x} are computed.  This functionality is the same as \code{\link[stats]{cor}} in \pkg{stats} except that it adds the formula notation \code{y~x}.
#'
#' @details see details in \code{\link[stats]{cor}}.  The formula version is only used with two variables in either the \code{y~x} or \code{~y+x} form.
#'
#' @param x A numeric vector, matrix, or data.frame (see details in \code{\link[stats]{cor}}) or a formula (see details here)
#' @param y A numeric vector, matrix, or data.frame (see details in \code{\link[stats]{cor}})
#' @param data An optional data frame that contains the variables in the formula.
#' @param use An optional character string that defines a method for computing the correlation in the presence of missing values. This must be one of \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}
#' @param method A single character string that indicates which correlation coefficient is to be computed. Must be one of \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}
#' @param digits A single numeric that indicates the number of decimals to which the result should be rounded.
#' @param \dots Unused.
#'
#' @return A single numeric if \code{x} and \code{y} are vectors or a numeric matrix if \code{x} and \code{y} are matrices.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}, but this is largely a wrapper to \code{\link[stats]{cor}} from \pkg{stats} that includes a formula version.
#'
#' @seealso See \code{\link[stats]{cor}}.
#'
#' @keywords htest
#'
#' @examples
#' ## example from stats::cor() ... all return equivalent
#' df <- data.frame(x=1:10,y=2:11)
#' cor(df$x,df$y)
#' corr(df$x,df$y)
#' corr(y~x,data=df)
#' corr(~x+y,data=df)
#' 
#' ## another example from stats::cor()
#' cor(longley)
#' corr(longley)
#' 
#' @rdname corr
#' @export
corr <- function (x, ...) {
  UseMethod("corr") 
}

#' @rdname corr
#' @export
corr.default <- function(x,y=NULL,use="everything",
                         method=c("pearson","kendall","spearman"),
                         digits=NULL,...) {
  if (!is.null(digits)) round(stats::cor(x,y,use,method),digits)
  else stats::cor(x,y,use,method)
}

#' @rdname corr
#' @export
corr.formula <- function(x,data,use="everything",
                         method=c("pearson","kendall","spearman"),
                         digits=getOption("digits"),...) {
  tmp <- iHndlFormula(x,data)
  ## Checks
  if (tmp$vnum<2) stop("Must have at least two variables.",call.=FALSE)
  if (any(!c(tmp$Rclass,tmp$Eclass) %in% c("numeric","integer"))) stop("All variables must be numeric.",call.=FALSE)
  ## Put in separate vectors if only two variables so that
  ##   only a single numbers is returned. Otherwise, leave
  ##   as a data.frame to get a matrix returned
  if (tmp$vnum==2) corr.default(tmp$mf[[1]],tmp$mf[[2]],use=use,method=method,digits=digits)
    else corr.default(tmp$mf,use=use,method=method,digits=digits)
}
