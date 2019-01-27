context("Plot two-way interactions")

#' @import data.table

dt_iris <- as.data.table(iris)
dt_iris[, Species := as.numeric(Species)]

p <- plot_twoway_interactions(data = dt_iris,
                         target_var = "Species",
                         top_n = 5)

test_that("Target covariance calculation works", {
  check <- round(p$results$covariance, digits = 7) == c(-0.2893403, -0.2892166,  0.5230585,  0.6357173,  0.5211915, -0.2202442)
  expect_true(all(check == TRUE))
})

test_that("Combined ranking works", {
  check <- p$results$combined_rank == c(6, 5, 4, 3, 2, 1)
  expect_true(all(check == TRUE))

  best_cmb <- p$results[min(combined_rank), name]
  expect_identical(best_cmb, "Petal.Length_&_Sepal.Width")
})

test_that("Plot uses expected geom", {
  expect_identical(class(p$plot$layers[[1]]$geom)[[1]], "GeomPoint")
})

test_that("Plot uses expected stat", {
  expect_identical(class(p$plot$layers[[2]]$stat)[[1]], "StatIdentity")
})
