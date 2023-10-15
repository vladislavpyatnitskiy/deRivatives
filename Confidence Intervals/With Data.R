# Calculate Confidence interval
confidence_intrvl <- function(stock_price, time_to_maturity,
                              interval_values = c(0.025, 0.975)){
  # Log returns
  volatility <- diff(log(stock_price))[-1,]
  
  # Calculate volatility
  volatility <- apply(volatility, 2, function(col) sd(col)) * (252 ^ 0.5) 
  
  # Select last price
  price_ci <- stock_price[nrow(stock_price),]
  
  # Calculate expected return
  expected_return <- as.matrix(stock_price[nrow(stock_price),]) /
    as.matrix(stock_price[nrow(stock_price)-252,]) - 1
  
  # Create empty variable to contain values for lower & upper bounds
  array_bound <- NULL
  
  # Create empty variable to contain values for lower & upper bounds
  for (m in 1:length(interval_values)){
    
    # Create empty variable to contain values for lower & upper bounds
    bound_values_list <- NULL
    
    # For each column 
    for (n in 1:ncol(stock_price)){
      
      # Calculate mean
      mean_for_ci <- log(price_ci[,n]) + 
        (expected_return[,n] - (((volatility[n]) ^ 2) / 2)) * time_to_maturity
      
      # Calculate sd
      sd_for_ci <- ((volatility[n]) ^ 2 * time_to_maturity) ^ 0.5
      
      # Calculate value for bound
      bound_values <- exp(mean_for_ci + qnorm(interval_values[m]) * sd_for_ci)
      
      # Add bound value to data frame
      bound_values_list <-rbind(bound_values_list, bound_values) }
    
    # Lower Bound & Upper Bound
    array_bound <- cbind(array_bound, bound_values_list) }
  
  # Give column names
  colnames(array_bound) <- c("Lower Bound", "Upper Bound")
  
  # Make it matrix
  array_bound <- as.matrix(array_bound)
  
  # Give row names
  rownames(array_bound) <- colnames(stock_price)
  
  # Display text
  return(array_bound)
}
# Test
confidence_intrvl(stock_price = stock_data, time_to_maturity = 0.25,
                  interval_values = c(0.025, 0.975))
