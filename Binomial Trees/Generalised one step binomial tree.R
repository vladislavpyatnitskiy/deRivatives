# Function to calculate option value via generalisation
gen_one_step_bin_tree <- function(time_to_maturity,
                                   orig_stock_price,
                                   delta_for_stock,
                                   interest_rate){
  
  # Return when stock goes up
  stock_price_up <- (orig_stock_price + delta_for_stock) / orig_stock_price
  
  # Return when stock goes down
  stock_price_down <- (orig_stock_price - delta_for_stock) / orig_stock_price
  
  # Calculate probability
  bin_pr <- (exp(interest_rate * time_to_maturity) - stock_price_down) /
    (stock_price_up - stock_price_down)
  
  # Actual option price
  bin_option_price <- exp(-interest_rate * time_to_maturity) *
    (1 * bin_pr + 0 * (1 - bin_pr))
  
  # Round
  bin_option_price <- round(bin_option_price, 3)
  
  # Put value in text
  bin_display <- sprintf("The value of the option today is %s",
                         bin_option_price)
  # Display
  return(bin_display)
}
# Test
gen_one_step_bin_tree(time_to_maturity = 0.25,
                       orig_stock_price = 20,
                       delta_for_stock = 2,
                       interest_rate = 0.12)
