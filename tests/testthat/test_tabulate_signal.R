context("Tabulate signal")

#' @import data.table

dt_iris <- as.data.table(iris)
dt_iris[, Species := as.numeric(Species)]
t <- tabulate_signal(dt_iris, target_var = "Species")

test_that("Target variance calculated correctly", {
  check <- round(t$variance, digits = 7) == c(0.6395973, 0.6395414, 0.4832801, 0.2156632)
  expect_true(all(check == TRUE))
})
