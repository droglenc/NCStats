#' Numbers of frogs on lakes as determined by call and visual methods.
#'
#' Hypothetical population of numbers of frogs on lakes as determined by call and visual methods.
#'
#' @name Frogs
#' 
#' @docType data
#'
#' @format A data frame with 1000 observations on the following 2 variables:
#'   \describe{
#'     \item{call}{Abundance determined from the \sQuote{call} method.}
#'     \item{visual}{Abundance determined from the \sQuote{visual} method.}
#'   }
#'   
#'@note Data were randomly determined using the following code:
#'
#' require(MASS)
#' round(mvrnorm(n=1000,c(120,225),matrix(c(25,18,18,35),2,2),empirical=TRUE),0)
#'
#' @source Randomly determined from realistic values provided by the Sigurd Olson Environmental Institute, Northland College, Ashland, WI.
#' 
#'@keywords datasets
NULL
