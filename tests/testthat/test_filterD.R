context("filterD() tests")

test_that("filterD() messages",{
  expect_error(filterD(0:5),"no applicable method")
  expect_error(filterD(matrix(0:5,ncol=2)),"no applicable method")
  expect_warning(filterD(iris,Species=="DEREK"),"resultant data.frame")
})  


test_that("filterD() results",{
  # limit to two groups
  grp <- c("setosa","versicolor")
  tmp <- filterD(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),100)
  # limit to one group
  grp <- c("versicolor")
  tmp <- filterD(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  # make sure that levels are not reordered
  iris$Species1 <- factor(iris$Species,levels=c("virginica","versicolor","setosa"))
  grp <- c("setosa","versicolor")
  tmp <- filterD(iris,Species1 %in% grp)
  expect_equal(levels(tmp$Species1),rev(grp))
  # check usage of except
  tmp <- filterD(iris,Species1 %in% grp,except="Species")
  expect_equal(levels(tmp$Species1),rev(grp))
  expect_equal(levels(tmp$Species),c("setosa","versicolor","virginica"))
})  

