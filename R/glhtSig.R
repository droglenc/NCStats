#'Extracts names for significantly different comparisons.
#'
#'Extracts names for significantly different comparisons from a \code{glht} object.
#'
#'@aliases glhtSig glhtSig.glht
#'
#'@param object An object saved from \code{glht()} from the \pkg{multcomp} package.
#'@param test A function for computing p-values.  See help for \code{glht}.
#'@param alpha A numeric indicated the rejection criterion to be used for identifying significant comparisons.  Defaults to \code{0.05}.
#'@param \dots Other arguments to pass through to \code{summary.glht()}.
#'
#'@return A vector containing the names of the paired comparisons that have a p-value less than \code{alpha}.
#'
#'@note This can be very slow to process.  Be patient.
#'
#'@seealso \code{glht} and \code{summary.glht} in \pkg{multcomp}.
#'
#'@keywords misc
#'
#'@examples
#'## NONE YET
#'
#'@rdname glhtSig
#'@export
glhtSig <- function (object, ...) {
  UseMethod("glhtSig") 
}

#'@rdname glhtSig
#'@export
glhtSig.glht <- function(object,test=adjusted(),alpha=0.05,...) {
  ts <- test(object)
  names(ts$coefficients)[ts$pvalues<alpha]
}
