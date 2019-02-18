#' @import data.table
#' @importFrom("caret", "createFolds")

context("Detect noise")

dt_iris <- as.data.table(iris)
dt_iris[, Species := as.numeric(Species)]
set.seed(2019)
t <- tabulate_noise(data = dt_iris,
                target_var = "Species",
                folds = caret::createFolds(dt_iris$Species, 5))

test_that("Average target correlation calculated correctly", {
  check <- round(t$avg_target_cor, digits = 7) == c(0.9800265, 0.9698712, 0.7972680, 0.3512119)
  expect_true(all(check == TRUE))
})
