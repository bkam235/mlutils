#' @import data.table

context("Ridgeline plot of all vars")

dt_iris <- as.data.table(iris)
p <- plot_feature_distributions(dt_iris, "Species")

test_that("Correct geom", {
  expect_equal(class(p$layers[[1]]$geom)[[1]], "GeomDensityRidges")
})
