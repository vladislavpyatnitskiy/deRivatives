# Function to calculte option value
one_step_binomial_tree <- function(time_to_maturity,
                          orig_stock_price,
                          delta_for_stock,
                          interest_rate,
                          option_value){
  
  # Price when stock goes up
  stock_price_up <- orig_stock_price + delta_for_stock
  
  # Price when stock goes down
  stock_price_down <- orig_stock_price - delta_for_stock
  
  # Calculate delta of the portfolio
  delta_bin <- option_value / (stock_price_up - stock_price_down)
  
  # Value of portfolio at change
  value_bin <- stock_price_up * delta_bin - option_value
  
  # Value of portfolio adjusting with interest rate and time
  portfolio_bin_value <- value_bin * exp(-interest_rate * time_to_maturity)
    
  # Actual option price
  bin_option_price <- orig_stock_price * delta_bin - portfolio_bin_value
  
  # Round
  bin_option_price <- round(bin_option_price, 3)
  
  # Put value in text
  bin_display <- sprintf("The value of the option today is %s",
                        bin_option_price)
  
  # Display
  return(bin_display)
}
# Test
one_step_binomial_tree(time_to_maturity = 0.25,
                       orig_stock_price = 20,
                       delta_for_stock = 2,
                       interest_rate = 0.12,
                       option_value = 1)
