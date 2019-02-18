#' Plotting function for ridgeline plot of features against target
#'
#' @export
#'
#' @import ggridges
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#'
#' @param data abc
#' @param target_var abc

plot_feature_distributions <- function(data, target_var){
  variable <- NULL
  value <- NULL

  if(class(data[[target_var]]) != "factor"){stop("target_var is not a factor")}

  p <- data %>%
    tidyr::gather(variable, value, -target_var) %>%
    ggplot(aes(y = as.factor(variable),
               fill = get(target_var),
               x = dplyr::percent_rank(value))) +
    ggridges::geom_density_ridges(alpha = 0.5) +
    labs(title = "Ridgeline plot of features against target",
         x = "Percent rank of feature value",
         y = "Features",
         fill = target_var)
  print(p)
}
