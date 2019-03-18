#' Target encoding of features
#'
#' @export
#'
#' @import data.table
#' @import glue
#'
#' @param data data.table with features and target variable
#' @param vars_to_encode features to encode
#' @param target_var target variable to encode features with
#' @param n_folds number of folds across which encodings are calculated
#' @param alpha smoothing parameter for encoding

encode_target <- function(data,
                          vars_to_encode = colnames(data)[-match(target_var, colnames(data))],
                          target_var,
                          n_folds = 5,
                          alpha = 0.1){

  data <- data[, (c(vars_to_encode, target_var)), with = F]
  data[[target_var]] <- as.numeric(data[[target_var]])
  folds <- caret::createFolds(y = data[[target_var]], k = n_folds)

  for(var in vars_to_encode){
    for(test_idx in folds){
      mean_target <- data[-test_idx, list(t_mean = mean(get(target_var), na.rm = T), n = .N), by = eval(var)]
      t_mean_global <- data[-test_idx, mean(get(target_var), na.rm = T)]
      mean_target[, t_mean_global := t_mean_global]

      mean_target[, encoding := (t_mean * n + t_mean_global * alpha) / (n + alpha)]

      data[test_idx, eval(glue::glue("{var}_tmean"))] <- mean_target$encoding[match(data[test_idx, ][[var]], mean_target[[var]])]
    }
    data[[var]] <- NULL
  }
  data[[target_var]] <- NULL
  return(data)
}

