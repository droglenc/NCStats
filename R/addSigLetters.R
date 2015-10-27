#' Places significance letters next to mean points on a fitPlot.
#' 
#' Places significance letters next to mean points on an active \code{\link[FSA]{fitPlot}} from a one-way or a two-way ANOVA.
#' 
#' The graphic of group means must be active for this function to place the characters next to the group mean points.  Typically this graphic is made with the \code{\link[FSA]{fitPlot}} function.
#' 
#' @param mdl A \code{lm} object or formula depicting an ANOVA
#' @param lets A vector of characters to be placed next to each group mean point
#' @param which A character string listing terms in the fitted model for which the means should be calculated and the letters placed
#' @param change.order A logical that is used to change the order of the factors in the \code{lm} object.  This is used to change which factor is plotted on the x-axis and which is used to connect means.  Used only with a two-way ANOVA and only if the same argument is used with the \code{fitPlot} function
#' @param pos A value or vector of values indicating the positiong to place the characters relative to each group mean point.  Note that \code{1}=below, \code{2}=left, \code{3}=above, and \code{4}=right
#' @param offset A value indicating a proportion of character widths to move the character away from each group mean point
#' @param col A single or vector of numeric or character representations of colors used for each character
#' @param cex A single or vector of numeric values used to represent the character expansion values
#' @param \dots Other arguments to be passed to the \code{text} function
#' 
#' @return None.  However, an active graphic is modified.
#' 
#' @note Students are often asked to determine which level means are different if a significant one-way or two-way ANOVA is encountered.  The results of these multiple comparisons are often illustrated by placing \dQuote{significance letters} next to mean points on an interaction of main effects plot.  In R this can be accomplished with a number of \code{text} calls.  This function, however, simplifies this process so that newbie students can create the graphic in an efficient and relatively easy manner (in conjunction with \code{\link[FSA]{fitPlot}}).  This function thus allows newbie students to interact with and visualize moderately complex linear models in a fairly easy and efficient manner.
#' 
#' @seealso \code{\link[FSA]{fitPlot}}.
#' 
#' @keywords hplot
#' 
#' @examples
#' require(FSA)  # for the Mirex dataframe
#' data(Mirex)
#' Mirex$year <- factor(Mirex$year)
#' 
#' ## one-way ANOVA
#' lm1 <- lm(mirex~year,data=Mirex)
#' anova(lm1)
#' # suppose multiple comparison results from following
#' \dontrun{
#' library(multcomp)
#' mc1 <- glht(lm1,mcp(year="Tukey"))
#' summary(mc1)
#' }
#' fitPlot(lm1,main="")
#' addSigLetters(lm1,c("a","a","a","a","ab","b"),pos=c(2,2,4,4,4,4))
#' 
#' # same example, but using cld() from multcomp
#' \dontrun{
#' ( sl1 <- cld(mc1) )
#' fitPlot(lm1,main="")
#' addSigLetters(lm1,lets=sl1,pos=c(2,2,4,4,4,4))
#' }
#' ## two-way ANOVA
#' lm2 <- lm(mirex~year*species,data=Mirex)
#' anova(lm2)
#' 
#' # suppose multiple comparison results from following
#' \dontrun{
#' mc2y <- glht(lm2,mcp(year="Tukey"))
#' summary(mc2y)
#' mc2s <- glht(lm2,mcp(species="Tukey"))
#' summary(mc2s)
#' }
#' op <- par(mfcol=c(1,2))
#' fitPlot(lm2,which="year",type="b",pch=19,ylim=c(0.05,0.35),main="")
#' addSigLetters(lm2,which="year",c("a","a","a","a","ab","b"),pos=c(2,2,4,4,4,4))
#' fitPlot(lm2,which="species",type="b",pch=19,ylim=c(0.05,0.35),main="")
#' addSigLetters(lm2,which="species",c("a","a"),pos=c(2,4))
#' par(op)
#' 
#' @export
addSigLetters <- function(mdl,lets,which,change.order=FALSE,pos=rep(2,length(mns)),
                                     offset=0.5,col=rep(1,length(mns)),cex=rep(0,length(mns)),...) {
  mdl <- iTypeoflm(mdl)
  if (mdl$type!="ONEWAY" & mdl$type!="TWOWAY")
    stop("\n addSigLetters only works with one or two factors in the model.",call.=FALSE)
  if (mdl$type=="ONEWAY" | (mdl$type=="TWOWAY" & !missing(which))) {
    ifelse(missing(which),ord <- 2, ord <- match(which,colnames(mdl$mf)))
    mns <- tapply(mdl$mf[,1],mdl$mf[,ord],mean)
    x <- 1:length(mns)
  } else {
    ifelse(change.order,ord <- c(3,2),ord <- c(2,3))
    mns <- tapply(mdl$mf[,1],mdl$mf[,ord[1]]:mdl$mf[,ord[2]],mean)
    x <- rep(1:length(levels(mdl$mf[,ord[1]])),each=length(levels(mdl$mf[,ord[2]])))
    if (is.ordered(mdl$mf[,ord[1]])) x <- levels(mdl$mf[,ord[1]])[x]
  }
  if (class(lets) == "cld") lets <- lets$mcletters$Letters
  if (length(lets) != length(mns)) stop("Length of lets vector is incorrect.",call.=FALSE)
  if (length(pos) == 1) pos <- rep(pos,length(mns))
    else if (length(pos) != length(mns)) stop("Length of pos vector is incorrect.",call.=FALSE)
  if (length(col) == 1) col <- rep(col,length(mns))
    else if (length(col) != length(mns)) stop("Length of col vector is incorrect.",call.=FALSE)
  if (length(cex) == 1) cex <- rep(cex,length(mns))
    else if (length(cex) != length(mns)) stop("Length of cex vector is incorrect.",call.=FALSE)
  graphics::text(x,mns,lets,pos=pos,offset=offset,col=col,cex=cex,...)  
}
