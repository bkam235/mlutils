#' Plotting function for evaluating feature combinations by concatenation
#'
#' @export
#'
#' @import data.table
#' @import purrr
#' @import ggplot2
#' @import forcats
#' @import magrittr
#' @import gtools
#'
#' @param data abc
#' @param target_var abc
#' @param top_n abc


plot_rank_combinations <- function(data, target_var, top_n = 10){

  `.` <- NULL
  name <- NULL
  avg_variance_rank <- NULL
  avg_variance <- NULL
  covariance_rank <- NULL
  covariance <- NULL
  combined_rank <- NULL
  highlight <- NULL
  var <- NULL
  cov <- NULL

  if(class(data[[target_var]]) == "factor"){
    f <- data[[target_var]]
    data[[target_var]] <- as.numeric(levels(f))[f]
    # print("Target converted to numeric")
  }

  get_target_avgs <- function(var){
    target_avg <- data[, .(target_avg = mean(get(target_var), na.rm = TRUE)), by = eval(var)]
    dt_var <- data[, .(get(var))]
    setnames(dt_var, var)
    dt_var_avg <- merge(dt_var, target_avg, by = var, all.x = TRUE)
    return(dt_var_avg$target_avg)
  }

  cols <- colnames(data)[-match(target_var, colnames(data))]
  cmb <- permutations(n=length(cols), r=2, v=cols)

  cmb_ll <- apply(cmb, 1, list) %>% flatten()
  duplicate_rows <- numeric()

  for(i in 1:nrow(cmb)){
    if(i %in% duplicate_rows){next}
    cmb_i <- cmb[i, ]
    matches <- map(cmb_ll, function(x){cmb_i %in% x}) %>%
      map(function(x){all(x == TRUE)}) %>%
      flatten_lgl() %>%
      which()
    duplicate_rows <- c(duplicate_rows, matches[-match(i, matches)])
  }

  cmb <- cmb[-duplicate_rows, ]

  cmb_l <- split(cmb, 1:nrow(cmb))
  cmb_l_names <- map(cmb_l, function(x){paste0(x[[1]], "_&_", x[[2]])}) %>% flatten_chr()
  names(cmb_l) <- cmb_l_names

  get_cmb_stats <- function(vars){
    dt_vars <- data.table(
      var1 = get_target_avgs(vars[[1]]),
      var2 = get_target_avgs(vars[[2]])
    )
    var1_variance <- var(dt_vars$var1, na.rm = TRUE)
    var2_variance <- var(dt_vars$var2, na.rm = TRUE)
    avg_variance <- mean(var1_variance, var2_variance)
    covariance <- cov(dt_vars$var1, dt_vars$var2, use = "pairwise.complete.obs")
    return(list(avg_variance = avg_variance, covariance = covariance))
  }

  future::plan("multisession")
  results_stats <- furrr::future_map(cmb_l, get_cmb_stats)
  results_dt <- transpose(results_stats) %>%
    map(flatten_dbl) %>%
    as.data.table()
  results_dt[, name := cmb_l_names]
  results_dt[, avg_variance_rank := rank(avg_variance)]
  results_dt[, covariance_rank := rank(covariance)]
  results_dt[, combined_rank := rank(avg_variance_rank - covariance_rank)]
  results_dt[, highlight := 0]
  results_dt <- results_dt[order(combined_rank, decreasing = TRUE), ]
  results_dt[1:top_n, highlight := 1]

  p <- ggplot(results_dt, aes(x = avg_variance_rank, y = covariance_rank, label = name)) +
    geom_jitter(color = ifelse(results_dt$highlight == 1, "red", "black")) +
    ggrepel::geom_text_repel(size = 3,
                    data = subset(results_dt, highlight == 1)) +
    scale_y_reverse() +
    labs(title = "Target average variance and covariance ranks",
         subtitle = glue::glue("Combinations: {length(results_stats)}, Highlighted: Top {top_n}"),
         x = "Target average variance rank (higher is better)",
         y = "Combination covariance rank (lower is better)")

  return(list(results = results_dt, plot = p))
}
