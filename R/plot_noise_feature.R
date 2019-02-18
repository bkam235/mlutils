#' Plot to evaluate variable robustness of single variable
#'
#' @export
#'
#' @import data.table
#' @import purrr
#' @import ggplot2
#'
#' @param data abc
#' @param target_var abc
#' @param var abc
#' @param folds abc

plot_noise_feature <- function(
  data,
  target_var,
  var,
  folds
){
  get_var_fold <- NULL
  `.` <- NULL
  target_avg.x <- NULL
  target_avg.y <- NULL
  # median <- NULL

  if(class(data[[target_var]]) == "factor"){
    f <- data[[target_var]]
    data[[target_var]] <- as.numeric(levels(f))[f]
    # print("Target converted to numeric")
  }

  get_var_fold <- function(data, var, fold_idx){
    train_data <- data[-fold_idx, ]
    test_data <- data[fold_idx, ]

    train_target_avg <- train_data[, .(target_avg = mean(get(target_var), na.rm = TRUE)), by = eval(var)]
    test_target_avg <- test_data[, .(target_avg = mean(get(target_var), na.rm = TRUE)), by = eval(var)]
    target_avg <- merge(train_target_avg, test_target_avg, by = var, all.x = T)
    # target_avg_cor <- cor(target_avg$target_avg.x, target_avg$target_avg.y, "complete.obs")
    return(target_avg)
  }

  fold_stats <- map(folds, get_var_fold, data = data, var = var) %>%
    map(function(fold){fold[, .(get(var), avg_diff = target_avg.x - target_avg.y)]})

  for(i in 1:length(fold_stats)){setnames(fold_stats[[i]], "avg_diff", paste0("avg_diff", i))}

  stats <- data.table(V1 = fold_stats[[1]][["V1"]])
  for(i in 1:length(fold_stats)){
    stats <- merge(stats, fold_stats[[i]], by = "V1", all = TRUE)
  }

  stats <- melt(stats, id.vars = "V1")
  setnames(stats, "V1", var)

  p <- ggplot(stats, aes_string(x = var, y = "value")) +
    stat_summary(
      fun.ymin = min,
      fun.ymax = max,
      fun.y = stats::median
    ) +
    labs(title = "Differences between target averages across folds",
         subtitle = "Min, Median, Max",
         x = var,
         y = "difference") +
    ylim(c(-1 * max(data[[target_var]]), 1 * max(data[[target_var]])))

  return(p)
}
