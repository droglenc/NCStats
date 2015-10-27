#' @title Extracts names for significantly different comparisons.
#' 
#' @description Extracts names for significantly different comparisons from a \code{glht} object.
#' 
#' @aliases glhtSig glhtSig.glht
#' 
#' @param object An object saved from \code{\link[multcomp]{glht}} from the \pkg{multcomp} package.
#' @param type A function for computing p-values (see \code{\link[multcomp]{glht}}).
#' @param alpha A numeric indicated the rejection criterion to be used for identifying significant comparisons.  Defaults to \code{0.05}.
#' @param \dots Other arguments to pass through to \code{\link[multcomp]{summary.glht}}.
#' 
#' @return A vector containing the names of the paired comparisons that have a p-value less than \code{alpha}.
#' 
#' @note This can be very slow to process.  Be patient.
#' 
#' @seealso \code{\link[multcomp]{glht}} and \code{\link[multcomp]{summary.glht}} in \pkg{multcomp}.
#' 
#' @keywords misc
#' 
#' @examples
#' ## NONE YET
#' 
#' @rdname glhtSig
#' @export
glhtSig <- function (object, ...) {
  UseMethod("glhtSig") 
}

#' @rdname glhtSig
#' @export
glhtSig.glht <- function(object,type=c("single-step","Shaffer","Westfall","free",
                                       stats::p.adjust.methods),alpha=0.05,...) {
  if(iChk4Namespace("multcomp")) {
    test <- multcomp::adjusted(type)
    ts <- test(object)
    names(ts$coefficients)[ts$pvalues<alpha]
  }
}
