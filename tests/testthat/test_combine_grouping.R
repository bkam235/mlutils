#' @import data.table

context("Combine grouping")

dt_iris <- as.data.table(iris)
cmb <- combine_grouping(data = dt_iris,
                        vars = c("Sepal.Width", "Petal.Length"),
                        group_vars = c("Sepal.Length", "Petal.Width"))

test_that("Combinations calculated for all group_vars", {
  expect_equal(ncol(cmb), 16)
})

test_that("Calculating stats for combinations", {
  colnames_check <- c(
    "Sepal.Width_mean_Sepal.Length",
    "Sepal.Width_sd_Sepal.Length",
    "Sepal.Width_min_Sepal.Length",
    "Sepal.Width_max_Sepal.Length",
    "Petal.Length_mean_Sepal.Length",
    "Petal.Length_sd_Sepal.Length",
    "Petal.Length_min_Sepal.Length",
    "Petal.Length_max_Sepal.Length",
    "Sepal.Width_mean_Petal.Width",
    "Sepal.Width_sd_Petal.Width",
    "Sepal.Width_min_Petal.Width",
    "Sepal.Width_max_Petal.Width",
    "Petal.Length_mean_Petal.Width",
    "Petal.Length_sd_Petal.Width",
    "Petal.Length_min_Petal.Width",
    "Petal.Length_max_Petal.Width"
  )
  expect_true(all(colnames(cmb) == colnames_check))
})

test_that("Stats calculated correctly", {
  row_check <- c(3.47777777777778, 0.411636301174282, 2.5, 3.8, 1.72222222222222,
                 0.504424865014052, 1.4, 3, 3.37931034482759, 0.307500650806312,
                 2.9, 4.2, 1.4448275862069, 0.172349741797129, 1, 1.9)
  expect_true(all(round(row_check, 7) == round(cmb[1, ], 7)))
})
