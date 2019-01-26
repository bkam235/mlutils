#' @import data.table
#' @import purrr
#' @import ggplot2
#' @import forcats
#' @import magrittr

get_all_variances <- function(data, target_var){
  variance <- NULL

  if(class(data[[target_var]]) == "factor"){
    f <- data[[target_var]]
    data[[target_var]] <- as.numeric(levels(f))[f]
    # print("Target converted to numeric")
  }

  get_variance <- function(var, data){
    `.` <- NULL
    target_avg <- data[, .(target_avg = mean(get(target_var), na.rm = TRUE)), by = eval(var)]
    dt_var <- data[, .(get(var))]
    setnames(dt_var, var)
    dt_var_avg <- merge(dt_var, target_avg, by = var, all.x = TRUE)
    target_variance <- dt_var_avg[, target_avg] %>% var(na.rm = TRUE)
    return(target_variance)
  }

  cols <- colnames(data)[-match(target_var, colnames(data))]
  var_names <- as.list(cols)
  names(var_names) <- cols

  var_result <- map(var_names, get_variance, data = data)
  var_dt <- data.table(name = names(var_result),
                       variance = flatten_dbl(var_result))
  var_dt <- var_dt[order(variance, decreasing = TRUE), ]
  return(var_dt)
}

plot_single_variances <- function(data, target_var, top_n = 20){
  name <- NULL

  if(top_n > (ncol(data) - 1)){top_n <- (ncol(data) - 1)}

  result <- get_all_variances(data, target_var)
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
  print(p)
}
