context("Plot signal")

#' @import data.table

dt_iris <- as.data.table(iris)
dt_iris[, Species := as.numeric(Species)]
p <- plot_signal(dt_iris, target_var = "Species")

test_that("Target average variance calculation works", {
  check <- round(p$data$variance, digits = 7) == c(0.6395973, 0.6395414, 0.4832801, 0.2156632)
  expect_true(all(check == TRUE))
})

test_that("Plot uses expected geom", {
  expect_identical(class(p$layers[[1]]$geom)[[1]], "GeomSegment")
})

test_that("Plot uses expected stat", {
  expect_identical(class(p$layers[[2]]$stat)[[1]], "StatIdentity")
})
