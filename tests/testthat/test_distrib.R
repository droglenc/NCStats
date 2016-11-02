context("distrib() tests")
test_that("distib() messages",{
  expect_error(distrib(1,lower.tail="FALSE"),"must be a logical")
})
