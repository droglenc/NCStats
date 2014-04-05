#'Perform Levene's test for homogeneity of variances
#'
#'Perform Levene's test for homogeneity of variances
#'
#'@param \dots Arguments to pass through to \code{leveneTest} in \pkg{car}.
#'@return See help for \code{leveneTest} in \pkg{car}.
#'@seealso \code{leveneTest} in \pkg{car}.
#'@note This is a simple pass-through to  \code{leveneTest} in \pkg{car} for those
#'of us that always put the \sQuote{s} on \sQuote{Levenes}.
#'@keywords misc
#'@export
#'
levenesTest <- function(...) leveneTest(...)