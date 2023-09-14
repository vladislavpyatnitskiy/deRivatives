# Function to calculate confidence intervals to stock volatility
confidence_interval <- function(stock_price,
                                expected_return,
                                volatility,
                                time_to_maturity){
  # Calculate
  mean_for_ci <- log(stock_price) + 
    (expected_return - ((volatility ^ 2) / 2)) *
    time_to_maturity
  
  # Calculate sd
  sd_for_ci <- (volatility ^ 2 * time_to_maturity) ^ 0.5
  
  # Calculate lower bound
  lower_bound <- exp(mean_for_ci + qnorm(0.025) * sd_for_ci)
  
  # Calculate upper bound
  upper_bound <- exp(mean_for_ci + qnorm(0.975) * sd_for_ci)
  
  # Put values into text
  text_for_ci <- sprintf("There is 95%% probability that the stock price will lie between %.2f and %.2f.",
                         lower_bound,
                         upper_bound)
  # Display text
  return(text_for_ci)
}
# Test
confidence_interval(stock_price = 40,
                    expected_return = 0.16,
                    volatility = 0.2,
                    time_to_maturity = 0.5)
