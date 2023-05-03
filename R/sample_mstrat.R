sample_mstrat <- function(sampling_data,
                          reference_data,
                          n,
                          alpha = 1){
  pdata <- sampling_data
  
  for(var in colnames(pdata)){
    frequencies <- reference_data[, .N, by = eval(var)]
    frequencies[, N := N / sum(N)]
    pdata[[var]] <- frequencies$N[match(pdata[[var]], frequencies[[var]])]
    pdata[is.na(get(var)), eval(var) := 0]
  }
  
  w <- rowSums(pdata) ^ alpha
  sample_idx <- sample(c(1:nrow(sampling_data)), n, prob = w)
  sampled_data <- sampling_data[sample_idx, ]
  
  return(sampled_data)
}