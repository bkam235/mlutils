#' Combine features by concatenation
#'
#' @export
#'
#' @import magrittr
#' @import data.table
#' @import purrr
#'
#' @param data abc
#' @param cmb_list abc

combine_concatenation <- function(data, cmb_list){
  `%>%` <- NULL

  create_combination <- function(cmb){
    vars <- map(cmb, function(x){data[[x]]})
    concat <- vars %>%
      transpose() %>%
      map(flatten_chr) %>%
      map(paste0, collapse = "_") %>%
      flatten_chr

    concat <- as.factor(concat)
    concat_name <- paste0(cmb, collapse = "_")

    return(list(concat_name, concat))
  }

  combinations <- map(cmb_list, create_combination) %>%
    transpose()

  cmb_dt <- as.data.table(combinations[[2]])
  setnames(cmb_dt, flatten_chr(combinations[[1]]))

  return(cmb_dt)
}
