#'Calculates the geometric mean or geometric standard deviation.
#'
#'Calculates the geometric mean or standard deviation of a vector of numeric
#'values.
#'
#'The geometric mean is computed by log transforming the raw data in \code{x},
#'computing the arithmetic mean of the transformed data, and back-transforming
#'this mean to the geometric mean by exponentiating.
#'
#'The geometric standard deviation is computed by log transforming the raw data
#'in \code{x}, computing the arithmetic standar deviation of the transformed
#'data, and back-transforming this standard deviation to the geometric standard
#'deviation by exponentiating.
#'
#'@aliases geomean geosd
#'@param x Vector of numeric values.
#'@param na.rm Logical indicating whether to remove missing values or not.
#'@param zneg.rm Logical indicating whether to ignore or remove zero or
#'negative values found in \code{x}.
#'@return A numeric value that is the geometric mean or geometric standard
#'deviation of the numeric values in \code{x}.
#'@note This function is largely an implementation of the code suggested by
#'Russell Senior on R-help in November, 1999.
#'@seealso \code{geometric.mean} in \pkg{psych}.
#'@keywords misc
#'@examples
#'## generate random lognormal data
#'d <- rlnorm(500,meanlog=0,sdlog=1)
#'# d has a mean on log scale of 0; thus, gm should be exp(0)~=1
#'# d has a sd on log scale of 1; thus, gsd should be exp(1)~=2.7
#'geomean(d)
#'geosd(d)
#'
#'## Examples from psych package -- gm matches results
#'x <- seq(1,5)
#'x2 <- x^2
#'geomean(x)
#'geomean(x2)
#'
#'## Demonstrate handling of zeroes and negative values
#'x <- seq(0,5)
#'# this will given an error
#'try(geomean(x))
#'# this will only give a warning, but might not be what you want
#'geomean(x,zneg.rm=TRUE)
#'
#'@rdname geomean
#'@export geomean
geomean <- function(x,na.rm=FALSE,zneg.rm=FALSE) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be a numeric vector.",call.=FALSE)
  if (any(x <= 0) & !zneg.rm) stop("'x' must contain all positive values.",call.=FALSE)
  if (any(x <= 0) & zneg.rm) {
    warning("Some non-positive values were ignored/removed.",call.=FALSE)
    x <- x[x>0]   # remove non-positive values
  }
  exp(mean(log(x),na.rm=na.rm))
}

#'@rdname geomean
#'@export geosd
geosd <- function(x,na.rm=FALSE,zneg.rm=FALSE) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be a numeric vector.",call.=FALSE)
  if (any(x <= 0) & !zneg.rm) stop("'x' must contain all positive values.",call.=FALSE)
  if (any(x <= 0) & zneg.rm) {
    warning("Some non-positive values were ignored/removed.",call.=FALSE)
    x <- x[x>0]   # remove non-positive values
  }
  exp(sd(log(x),na.rm=na.rm))
}
