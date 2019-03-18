#' @import data.table

context("Encode target")

dt_iris <- as.data.table(iris)
set.seed(2019)
enc <- encode_target(data = dt_iris,
                     vars_to_encode = "Sepal.Length",
                     target_var = "Species",
                     n_folds = 3,
                     alpha = 0.1)

test_that("Returns only encoded features", {
  expect_true(colnames(enc) == "Sepal.Length_tmean")
})

test_that("Encoding calculated correctly", {
  check <- c(1.01843137254902, 2.00161290322581, 1.09545454545455, 1.03387096774194,
             1.21666666666667)
  test <- round(enc$Sepal.Length_tmean[1:5], 14)
  expect_true(all(check == test))
})
