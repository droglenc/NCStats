#'Tests for significant differences among all pairs of intercepts in an IVR.
#'
#'Tests for significant differences among all pairs of intercepts in an
#'indicator variable regression where the indicator variables all stem from one
#'factor.
#'
#'In an indicator variable regression without the interaction(s) between the
#'covariate (x) and indicator variable(s) (i.e., parallel lines) the
#'coefficient on the indicator variables tests for a difference in interceps
#'between the level of the indicator variable and the reference level.  Thus,
#'all indicator variables from a particular linear model fit only compare
#'intercepts with the reference level.  Other intercept comparisons can be
#'found by changing the reference level but this requires re-fitting the model.
#'Alternatively, Tukey's HSD method of multiple comparisons can be used but
#'this requires adjusting the original observations as if the original
#'observations were all collected at the exact same value of the covariate (x).
#'Because of the required adjustment the \code{TukeyHSD} function is
#'inappropriate for testing for difference in intercepts in an indicator
#'variable regression.
#'
#'This function provides a statistical comparison of all pairs of intercepts by
#'first adjusting the observed data to a common value of the covariate
#'(\code{common.cov}), computing a one-way ANOVA to determine if the mean
#'adjusted values differ by level of group factor in the original IVR, and then
#'submitting the one-way ANOVA results to the \code{TukeyHSD} function to
#'determine for which levels the mean adjusted values differ.  The levels for
#'which the mean adjusted values differ are also the levels for which the
#'intercepts differ.
#'
#'The default is to compute the adjusted values at the mean value of the
#'covariate (i.e., \code{common.cov=mean(x)}.  However, if interest is in the
#'intercepts (i.e., at X=0) then \code{common.cov=0} should be used instead.
#'
#'@aliases compIntercepts print.compIntercepts
#'@param mdl a \code{lm} object.
#'@param common.cov a value to be used as the common value of the covariate in
#'the adjustment process.  See details.
#'@param alpha a decimal numeric indicating the desired level for the
#'experimentwise error rate
#'@param digits A numeric that controls the number of digits to print.
#'@param x A \code{compIntercepts} object (i.e., returns from \code{compIntercepts}).
#'@param \dots Other arguments to be passed to the \code{TukeyHSD} or \code{print} functions.
#'@return A list with four components
#'\enumerate{
#'\item The comparison results as returned from the \code{TukeyHSD} function;
#'\item The value of the common covariate sent in \code{common.cov};
#'\item A vector of values of the resonse variable adjusted to the \code{common.cov}
#'value of the covariate.  This vector can be appended to the original data frame
#'to construct summary statistics for the adjusted values (e.g., mean adjusted value
#'for each group); and 
#'\item A vector of mean adjusted values at the value of the common covariate.
#'}
#'
#'The \code{print} function prints the comparison and adjusted means in a nice format.
#'@seealso \code{TukeyHSD} and \code{fitPlot} from \pkg{FSA}.
#'@keywords htest
#'@examples
#'require(FSA)      # for fitPlot and Mirex data
#'
#'data(Mirex)
#'Mirex <- Mirex[Mirex$year!="1996" & Mirex$year!="1999",]
#'Mirex$year <- factor(Mirex$year)
#'lm1 <- lm(mirex~weight+year,data=Mirex)
#'fitPlot(lm1)
#'compIntercepts(lm1)
#'res <- compIntercepts(lm1,common.cov=mean(Mirex$weight))
#'tapply(res$adjvals,Mirex$year,Mirex$summary)  # summarized adjusted values by year
#'
#'@rdname compIntercepts
#'@export compIntercepts
compIntercepts <- function(mdl,common.cov=mean(x),alpha=0.05,digits=getOption("digits"),...) {
  conf.level <- 1-alpha
  lmtype <- FSA:::typeoflm(mdl)
  if (lmtype$type!="IVR") stop("\nFunction only works for an indicator variable regression.",call.=FALSE)
  if (dim(mdl$model)[2]>3) stop("\nFunction only works for a one-way indicator variable\n regression (not a multiple regression of two-way IVR)!",call.=FALSE)
  if (dim(mdl$model)[2]==3) warning("\nModel contains an interaction term which will be\n removed (i.e., assume parallel lines) for testing intercepts.\n",call.=FALSE)
  y <- lmtype$mf[,1]
  if (is.factor(lmtype$mf[,2])) {
    g <- lmtype$mf[,2]
    x <- lmtype$mf[,3]
  } else {
    x <- lmtype$mf[,2]
    g <- lmtype$mf[,3] 
  }
  lm1 <- lm(y~x+g)
  adjvals <- predict(lm1,data.frame(x=rep(common.cov,dim(lmtype$mf)[1]),g=g))+lm1$residuals
  thsd <- TukeyHSD(aov(adjvals~g),...)
  cdf <- data.frame(comparison=rownames(thsd[[1]]),thsd[[1]])
  rownames(cdf) <- 1:dim(cdf)[1]
  res <- list(comparisons=cdf,common.cov=common.cov,adjvals=adjvals,means=tapply(adjvals,g,mean),digits=digits)
  class(res) <- "compIntercepts"
  res
}

#'@rdname compIntercepts
#'@method print compIntercepts
#'@S3method print compIntercepts
print.compIntercepts <- function(x,...) {
  cat("\nTukey HSD on adjusted means assuming parallel lines.\n")
  print(x$comparisons,digits=x$digits)
  cat("\nMean adjusted values at a covariate value of",formatC(x$common.cov,format="fg",digits=x$digits),"\n")
  print(x$means,digits=x$digits)
}
