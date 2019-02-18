#' Frequency encoding
#'
#' @export
#' @import data.table
#'
#' @param data abc
#' @param target_var abc
#' @param vars_to_encode abc
#' @param relative abc

encode_frequency <- function(data,
                          target_var,
                          vars_to_encode = colnames(data)[-match(target_var, colnames(data))],
                          relative = F){
  `.` <- NULL
  N <- NULL

  for(var in vars_to_encode){
    frequencies <- data[, .N, by = eval(var)]
    if(relative){frequencies[, N := N / sum(N)]}
    data[[var]] <- frequencies$N[match(data[[var]], frequencies[[var]])]
  }

  return(data)
}
