
test_that("iTypeoflm() messages and results",{
  ## Get data
  Mirex$year <- factor(Mirex$year)
  ## Check return types
  tmp <- lm(mirex~weight*year*species,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("IVR","list"))
  tmp <- lm(mirex~weight*year,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("IVR","list"))
  tmp <- lm(mirex~weight+year,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("IVR","list"))
  tmp <- lm(mirex~weight,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("SLR","list"))
  tmp <- lm(mirex~year,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("ONEWAY","list"))
  tmp <- lm(mirex~year*species,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("TWOWAY","list"))
  tmp  <- lm(mirex~weight+I(weight^2),data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("POLY","list"))
  tmp <- lm(mirex~weight+rnorm(nrow(Mirex)+rnorm(nrow(Mirex))),data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("MLR","list"))
  ## Check some errors
  glm1 <- glm(year~weight,data=Mirex,family="binomial")
  expect_error(FSA:::iTypeoflm(glm1),"only works with")
  nl1 <- nls(mirex~B1/(1+exp(B2+B3*weight)),start=list(B1=0.4,B2=2,B3=-0.5),
             data=Mirex)
  expect_error(FSA:::iTypeoflm(nl1),"only works with")
  Mirex$speciesZ <- as.character(Mirex$species)
  Mirex$yearZ <- as.character(Mirex$year)
  tmp <- lm(mirex~weight*speciesZ,data=Mirex)
  expect_warning(FSA:::iTypeoflm(tmp),"variable is a 'character'")
  tmp <- lm(mirex~yearZ*speciesZ,data=Mirex)
  expect_warning(FSA:::iTypeoflm(tmp),"variable is a 'character'")
})
