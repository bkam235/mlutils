#' Combine features by grouping statistics
#'
#' @export
#'
#' @import data.table
#' @import purrr
#' @import magrittr
#'
#' @param data A data.table with features to combine
#' @param vars Character vector features to combine, must be column names in data
#' @param group_vars Character vector with features to calculate grouping statistics, must be column names of numeric variables in data

combine_grouping <- function(data, vars, group_vars){

  setNames <- NULL
  ave <- NULL
  sd <- NULL

  if(any((class(data) == c("data.table", "data.frame")) == FALSE)){stop("data is not a data.table")}
  if(any((vars %in% colnames(data)) == FALSE)){stop("vars are not all column names in data")}

  group_vars <- setNames(as.list(group_vars), group_vars)
  vars <- setNames(as.list(vars), vars)
  grp_classes <- map(vars, function(x){class(data[[x]])}) %>% flatten_chr()
  if(any(grp_classes != "numeric")){stop("At least one of vars is not numeric")}

  grp_mean <- function(x, grp) ave(x, grp, FUN = function(x) mean(x, na.rm = TRUE))
  grp_sd <- function(x, grp) ave(x, grp, FUN = function(x) sd(x, na.rm = TRUE))
  grp_min <- function(x, grp) ave(x, grp, FUN = function(x) min(x, na.rm = TRUE))
  grp_max <- function(x, grp) ave(x, grp, FUN = function(x) max(x, na.rm = TRUE))

  get_var_stats <- function(var, grp_var){
    l <- list(
      grp_mean(data[[var]], data[[grp_var]]),
      grp_sd(data[[var]], data[[grp_var]]),
      grp_min(data[[var]], data[[grp_var]]),
      grp_max(data[[var]], data[[grp_var]])
    )

    l_names <- c(
      paste0(var, "_mean_", grp_var),
      paste0(var, "_sd_", grp_var),
      paste0(var, "_min_", grp_var),
      paste0(var, "_max_", grp_var)
    )

    l <- setNames(l, l_names)

    return(l)
  }

  get_grp_stats <- function(grp_var, vars){
    var_stats <- map(vars, get_var_stats, grp_var = grp_var)
    return(var_stats)
  }

  grp_stats <- map(group_vars, get_grp_stats, vars = vars)
  grp_dt <- flatten_dfc(grp_stats) %>% as.data.table()
  return(grp_dt)
}
