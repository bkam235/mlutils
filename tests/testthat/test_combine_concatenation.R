#' @import data.table
#' @importFrom (stringr, str_split)
#' @importFrom (purrr, map)
#' @importFrom (purrr, flatten_dbl)

context("Combine by concatenation")

iris_dt <- as.data.table(iris)
cmb_iris <- combine_concatenation(data = iris_dt,
                                  cmb_list = list(c("Sepal.Length", "Sepal.Width"),
                                                  c("Sepal.Length", "Sepal.Width", "Species")))

split1 <- stringr::str_split(cmb_iris$Sepal.Length_Sepal.Width, "_")
split1_length <- unique(purrr::flatten_dbl(purrr::map(split1, length)))

split2 <- stringr::str_split(cmb_iris$Sepal.Length_Sepal.Width_Species, "_")
split2_length <- unique(purrr::flatten_dbl(purrr::map(split2, length)))

test_that("Return data only contains combined features", {
  expect_equal(ncol(cmb_iris), 2)
})

test_that("Returned combinations follows naming convetion", {
  expect_equal(colnames(cmb_iris), c("Sepal.Length_Sepal.Width", "Sepal.Length_Sepal.Width_Species"))
})

test_that("Concat of 2 numerical features has correct length and values", {
  expect_equal(length(split1_length), 1)
  expect_equal(split1_length, 2)
  expect_match(as.character(cmb_iris$Sepal.Length_Sepal.Width[[1]]), "5.100000_3.500000")
})

test_that("Concat of 2 numerical and 1 factor features has correct length and values", {
  expect_equal(length(split2_length), 1)
  expect_equal(split2_length, 3)
  expect_match(as.character(cmb_iris$Sepal.Length_Sepal.Width_Species[[1]]), "5.100000_3.500000_1")
})
