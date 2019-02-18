#' Plot to evaluate feature informativeness
#'
#' @export
#'
#' @import ggplot2
#' @import forcats
#' @import magrittr
#'
#' @param data abc
#' @param target_var abc
#' @param top_n abc

plot_signal <- function(data, target_var, top_n = 20){
  name <- NULL

  if(top_n > (ncol(data) - 1)){top_n <- (ncol(data) - 1)}

  result <- tabulate_signal(data, target_var)
  result[, name := as.factor(name)]
  result$name <- fct_reorder(result$name, result$variance)
  p <- ggplot(result[1:top_n, ], aes_string(x = "name", y = "variance")) +
    geom_segment(aes_string(x = "name",
                     xend = "name",
                     y = 0,
                     yend = "variance")) +
    geom_point() +
    coord_flip() +
    labs(title = "Variances of target averages for unique variable values",
         y = "variance",
         x = "variable")
  return(p)
}
