#' @import data.table

context("Frequency encoding")

dt_iris <- as.data.table(iris)
t <- encode_frequency(data = dt_iris,
                      target_var = "Species")

test_that("Frequency encoding works", {
  check <- t[1, ]
  check_lgl <- check == c(9, 6, 13, 29, "setosa")
  expect_true(all(check_lgl == TRUE))
})
