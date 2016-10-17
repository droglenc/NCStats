context("corr.formula() tests")
test_that("corr.formula() messages",{
  expect_error(corr(Sepal.Length~Species,data=iris),"must be numeric")
  expect_error(corr(~Sepal.Length+Species,data=iris),"must be numeric")
  expect_error(corr(Species~Sepal.Length,data=iris),"must be numeric")
  expect_error(corr(~Species+Sepal.Length,data=iris),"must be numeric")
  expect_error(corr(~Species+Sepal.Length+Sepal.Width,data=iris),"must be numeric")
  expect_error(corr(~Sepal.Length,data=iris),"at least two variables")
})

test_that("corr.formula() types",{
  # Two variables ... returns numeric vector of 1
  tmp <- corr(~Sepal.Length+Sepal.Width,data=iris)
  expect_true(is.numeric(tmp))
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),1)
  # More than two variables ... returns numeric matrix
  tmp <- corr(~Sepal.Length+Sepal.Width+Petal.Length,data=iris)
  expect_true(is.numeric(tmp))
  expect_true(is.matrix(tmp))
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
})

test_that("corr.formula() results",{
  # match cor() results
  expect_equal(corr(~Sepal.Length+Sepal.Width,data=iris,digits=7),
               round(cor(iris$Sepal.Length,iris$Sepal.Width),7))
  expect_equal(corr(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,digits=7),
               round(cor(iris[,-5]),7))
})
