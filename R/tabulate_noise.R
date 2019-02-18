#' Evaluate variable robustness of multiple variables
#'
#' @export
#'
#' @import data.table
#' @import purrr
#'
#' @param data abc
#' @param target_var abc
#' @param vars abc
#' @param folds abc

tabulate_noise <- function(
  data,
  target_var,
  vars = colnames(data)[-match(target_var, colnames(data))],
  folds
){

  `.` <- NULL
  V1 <- NULL
  avg_target_cor <- NULL

  if(class(data[[target_var]]) == "factor"){
    f <- data[[target_var]]
    data[[target_var]] <- as.numeric(levels(f))[f]
    # print("Target converted to numeric")
  }

  vars <- as.list(vars)
  names(vars) <- vars

  get_var_fold <- function(data, var, fold_idx){
    train_data <- data[-fold_idx, ]
    test_data <- data[fold_idx, ]

    train_target_avg <- train_data[, .(target_avg = mean(get(target_var), na.rm = TRUE)), by = eval(var)]
    test_target_avg <- test_data[, .(target_avg = mean(get(target_var), na.rm = TRUE)), by = eval(var)]
    target_avg <- merge(train_target_avg, test_target_avg, by = var, all.x = T)
    target_avg_cor <- stats::cor(target_avg$target_avg.x, target_avg$target_avg.y, "complete.obs")
    return(target_avg_cor)
  }

  get_var <- function(var, folds){
    map(folds, get_var_fold, data = data, var = var) %>%
      flatten_dbl() %>%
      mean(na.rm = T)
  }

  res <- map(vars, get_var, folds = folds)
  res_dt <- as.data.table(res) %>% data.table::transpose()
  res_dt <- res_dt[, .(name = names(res), avg_target_cor = V1)][order(avg_target_cor, decreasing = TRUE), ]

  return(res_dt)
}
