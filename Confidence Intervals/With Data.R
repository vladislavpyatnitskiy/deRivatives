# Calculate Confidence interval
confidence_intrvl <- function(stock_price, expected_return, time_to_maturity){
  
  # Log returns
  volatility <- diff(log(stock_price))[-1,]
  
  # Calculate volatility
  volatility <- apply(volatility, 2, function(col) sd(col))
  
  # Select last price
  last_price_ci <- stock_price[nrow(stock_price),]
  
  # Create empty variable to contain values for lower & upper bounds
  array_lower_bound <- NULL
  array_upper_bound <- NULL
  
  # For each column 
  for (n in 1:ncol(stock_price)){
    
    # Calculate
    mean_for_ci <- log(last_price_ci[,n]) + 
      (expected_return - (((volatility[n]) ^ 2) / 2)) * time_to_maturity
    
    # Calculate sd
    sd_for_ci <- ((volatility[n]) ^ 2 * time_to_maturity) ^ 0.5
    
    # Calculate lower bound & upper bound
    lower_bound <- exp(mean_for_ci + qnorm(0.025) * sd_for_ci)
    upper_bound <- exp(mean_for_ci + qnorm(0.975) * sd_for_ci)
    
    # Lower Bound & Upper Bound
    array_lower_bound <- rbind(array_lower_bound, lower_bound)
    array_upper_bound <- rbind(array_upper_bound, upper_bound) }
  
  # Display table
  text_for_ci <- cbind(array_lower_bound, array_upper_bound)
  
  # Give column names
  colnames(text_for_ci) <- c("Lower Bound", "Upper Bound")
  
  # Make it matrix
  text_for_ci <- as.matrix(text_for_ci)
  
  # Give row names
  rownames(text_for_ci) <- colnames(stock_price)
  
  # Display text
  return(text_for_ci)
}
# Test
confidence_intrvl(stock_price = stock_data, expected_return = 0.01,
                    time_to_maturity = 0.25)
