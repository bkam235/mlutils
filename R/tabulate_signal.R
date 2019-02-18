#' Table to evaluate feature informativeness
#'
#' @export
#'
#' @import data.table
#' @import purrr
#'
#' @param data abc
#' @param target_var abc

tabulate_signal <- function(data, target_var){
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
