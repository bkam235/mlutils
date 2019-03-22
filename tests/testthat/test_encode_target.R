#' @import data.table

context("Encode target")

dt_iris <- as.data.table(iris)
set.seed(2019)
enc <- encode_target(data = dt_iris,
                     vars_to_encode = "Sepal.Length",
                     target_var = "Species",
                     n_folds = 3,
                     alpha = 0.1)[[1]]

enc2 <- encode_target(data = dt_iris[1:100, ],
                      val_data = dt_iris[101:150, -"Species"],
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

test_that("Encoding does not create NAs", {
  expect_true(all(is.na(enc$Sepal.Length_tmean)) == FALSE)
})

test_that("Val set encoding returns list of 2", {
  expect_true(length(enc2) == 2)
})

test_that("Val set encoding does not create NAs", {
  expect_true(all(is.na(enc2[[2]]$Sepal.Length_tmean)) == FALSE)
})

test_that("Val set encoding calculated correctly", {
  check <- c(1.98387096774194, 1.74390243902439, 1.57048915546685, 1.98387096774194,
             1.95454545454545)
  test <- round(enc2[[2]]$Sepal.Length_tmean[1:5], 14)
  expect_true(all(check == test))
})


