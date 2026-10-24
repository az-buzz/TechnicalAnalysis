stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  
  k_line <- sma(k_values[!is.na(k_values)], k_period)
  d_line <- sma(k_line, d_period)
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}