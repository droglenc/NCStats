#' Add Regression Line Equation and R-Square to a ggplot.
#' 
#' @description Add regression line equation and R^2 to a ggplot. Regression model is fitted using the function \code{\link[stats]{lm}}.
#' 
#' @inheritParams ggplot2::layer
#'
#' @param formula a formula object
#' @param label.x.npc,label.y.npc can be \code{numeric} or \code{character} vector of the same length as the number of groups and/or panels. If too short they will be recycled. 
#' \itemize{ 
#'   \item If \code{numeric}, value should be between 0 and 1. Coordinates to be used for positioning the label, expressed in "normalized parent coordinates".
#'   \item If \code{character}, allowed values include: i) one of c('right', 'left', 'center', 'centre', 'middle') for x-axis; ii) and one of c('bottom', 'top', 'center', 'centre', 'middle') for y-axis.}  If too short they will be recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used for absolute positioning of the label. If too short they will be recycled.
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or \code{\link[ggplot2:geom_text]{geom_label}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If TRUE silently removes missing values.
#' @param cf.digits Number of decimals for the regression line coefficients. Can be a vector as long as the coefficients.
#' @param rr.digits Number of decimals for R^2 value.
#'
#'@references This function is a modification of \code{stat_regline_equation} in \pkg{ggpubr}, but includes \code{cf.digits} and \code{rr.digits}.
#'
#' @section Computed variables:
#'   \describe{ \item{x}{x position for left edge}
#'   \item{y}{y position near upper edge}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{hjust}{Set to zero to override the default of the "text" geom.}}
#' @examples
#' p <- ggplot2::ggplot(data=iris,mapping=ggplot2::aes(x=Sepal.Length,y=Petal.Length)) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_smooth(method="lm") +
#'   theme_NCStats()
#' p + stat_SLR_equation()
#' p + 
#'   stat_SLR_equation(ggplot2::aes(label=..eq.label..),label.x.npc=0,label.y.npc=1.0) +
#'   stat_SLR_equation(ggplot2::aes(label=..rr.label..),label.x.npc=0,label.y.npc=0.9)
#' 
#' p2 <- p + ggplot2::facet_wrap(~Species)
#' p2 + stat_SLR_equation()
#' p2 + stat_SLR_equation(cf.digits=2)
#' p2 + stat_SLR_equation(cf.digits=c(3,4))
#' p2 + 
#'   stat_SLR_equation(ggplot2::aes(label=..eq.label..),
#'                     label.x.npc=0,label.y.npc=1.0,cf.digits=c(3,4)) +
#'   stat_SLR_equation(ggplot2::aes(label=..rr.label..),
#'                     label.x.npc=0,label.y.npc=0.9,rr.digits=3)
#'
#'@export
stat_SLR_equation <- function(
  mapping=NULL, data=NULL, formula=y~x,
  label.x.npc="left", label.y.npc="top", label.x=NULL, label.y=NULL,
  geom="text", position="identity",
  na.rm=FALSE, show.legend=NA, cf.digits=2, rr.digits=2,
  inherit.aes=TRUE, ...) {
  ggplot2::layer(
    stat=StatReglineEquation, data=data, mapping=mapping, geom=geom,
    position=position, show.legend=show.legend, inherit.aes=inherit.aes,
    params=list(formula=formula,
                label.x.npc=label.x.npc, label.y.npc=label.y.npc,
                label.x=label.x, label.y=label.y,parse=TRUE,
                na.rm=na.rm,cf.digits=cf.digits,rr.digits=rr.digits,
                ...))
}


StatReglineEquation <-
  ggplot2::ggproto("StatReglineEquation", ggplot2::Stat,
          required_aes=c("x", "y"),
          default_aes=ggplot2::aes(label=..eq.label..,hjust=..hjust..,vjust=..vjust..),
          compute_group=function(data,scales,formula,label.x.npc,label.y.npc,
                                 label.x,label.y,cf.digits,rr.digits) {
            force(data)
            if (length(unique(data$x)) < 2) {
              return(data.frame()) # Not enough data to perform test
            }
            .test <- .stat_lm(formula, data,cf.digits,rr.digits)
            # Returns a data frame with label: x, y, hjust, vjust
            .label.pms <- ggpubr:::.label_params(data=data, scales=scales,
                                                 label.x.npc=label.x.npc,
                                                 label.y.npc=label.y.npc,
                                                 label.x=label.x, label.y=label.y)
            .label.pms$hjust <- 0
            cbind(.test, .label.pms)
          }
  )


# Compute regression line equation
.stat_lm <- function(formula, data,cf.digits,rr.digits) {
  res.lm <- stats::lm(formula, data)
  coefs <- round(stats::coef(res.lm),digits=cf.digits)
  rr <- round(summary(res.lm)$r.squared,rr.digits)

  # Build model equation
  eq.char <- paste0(coefs[[2]],"*x",ifelse(coefs[[1]]<0,"-","+"),abs(coefs[[1]]))
  eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^\\1", eq.char)
  z <- data.frame(eq.label=gsub("x", "~italic(x)",
                                paste0("italic(y)~`=`~",eq.char), fixed=TRUE),
                  rr.label=paste("italic(R)^2", rr, sep="~`=`~"),
                  rr=rr)
  z
}
