#' @import data.table
#' @importFrom("caret", "createFolds")

context("Plot noise")

dt_iris <- as.data.table(iris)
dt_iris[, Species := as.numeric(Species)]
set.seed(2019)
p <- plot_noise_feature(data = dt_iris,
                target_var = "Species",
                var = "Sepal.Length",
                folds = caret::createFolds(dt_iris$Species, 5))

test_that("Calculation of difference across folds works", {
  check <- round(p$data[Sepal.Length == 5, value], digits = 7) == c(-0.3750000, -0.3750000,  0.2222222,  0.2222222,  0.3333333)
  expect_true(all(check == TRUE))
})

test_that("Calculation for every var value and fold", {
  ref_length <- length(unique(iris$Sepal.Length)) * 5
  expect_true(nrow(p$data) == ref_length)
})

test_that("Plot uses expected geom", {
  expect_identical(class(p$layers[[1]]$geom)[[1]], "GeomPointrange")
})

test_that("Plot uses expected stat", {
  expect_identical(class(p$layers[[1]]$stat)[[1]], "StatSummary")
})
