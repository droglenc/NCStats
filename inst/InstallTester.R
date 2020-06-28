library(NCStats)

## test of FSA (and some car) functions
data(ABDLakes)
tmp <- filterD(ABDLakes,county %in% c("Ashland","Bayfield"))
slr <- lm(area~dep.max,data=tmp)
ivr <- lm(area~dep.max*county,data=tmp)
aov1 <- lm(area~county,data=tmp)
aov2 <- lm(area~county*waterbody.type,data=tmp)
fitPlot(slr)
fitPlot(ivr)
fitPlot(aov1)
fitPlot(aov2)
residPlot(slr)
residPlot(ivr)
residPlot(aov1)
residPlot(aov2)
predictionPlot(slr,data.frame(dep.max=10))
transChooser(slr)  # select "show test results" for full testing
transChooser(ivr)  # select "show test results" for full testing
transChooser(aov1) # select "show test results" for full testing
transChooser(aov2) # select "show test results" for full testing
gofCI(chisq.test(c(A=56,B=34)))
ciSim()
accuracyPrecision()
iqrCalc(1:7)
iqrCalc(1:8)
distrib(1,shade.trans=0.5)

## test of car functions
levenesTest(aov1)
outlierTest(aov1)

## test of MASS functions
corrSim()

## test of multcomp functions
library(multcomp)
aov1a <- lm(dep.max~county,data=ABDLakes)
glht1a <- glht(aov1a,mcp(county="Tukey"))
glhtSig(glht1a)

## test of nortest functions
adTest(slr$residuals)

## test of plotrix functions
tmp2 <- filterD(tmp,area>500)
plot(area~dep.max,data=tmp2)
highlight(area~dep.max,data=tmp2,lbls=county)

## test of TeachingDemos functions
df <- data.frame(x=rnorm(25,100,5))
z.test(df$x,mu=99,sd=5)

## test of TeachingDemos functions
t.test(~x,data=df,mu=99)

## Make sure all dependent and imported packages would load
library(car)
library(ggplot2)
library(MASS)
library(mosaic)
library(multcomp)
library(nortest)
library(plotrix)
library(TeachingDemos)
library(withr)
library(manipulate)
