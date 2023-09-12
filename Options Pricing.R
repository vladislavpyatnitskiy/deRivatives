delta_calculator <- function(x,
                             strike_price_1,
                             interest_rate,
                             time_to_maturity){
  
  # Define table for calls
  call_table <- NULL
  
  # Define table for puts
  put_table <- NULL
  
  # For each column
  for (n in 1:(ncol(x))){
    
    # Extract recent observations
    stock_price <- (x[,n])[nrow(x),]
    
    # Calculate strike price
    strike_price <- strike_price_1 + stock_price
    
    # Calculate returns
    rtrns_delta <- diff(log(x[,n]))[-1,]
    
    # Calculate standard deviation
    sd_for_delta <- apply(rtrns_delta,
                          2,
                          function(col) sd(rtrns_delta))
    
    # Calculate first delta  
    delta_1 <- (log(stock_price/strike_price) +
                  (interest_rate + ((sd_for_delta^2)/2)) * time_to_maturity) /
      (sd_for_delta * (time_to_maturity)^0.5)
    
    # Calculate second delta
    delta_2 <- delta_1 - (sd_for_delta * ((time_to_maturity)^0.5))
    
    # Calculate option call price
    option_price_call <- stock_price *
      pnorm(delta_1) -
      strike_price *
      exp(-interest_rate * time_to_maturity) *
      pnorm(delta_2)
    
    option_price_call <- round(option_price_call, 2)
    
    # Calculate option put price
    option_price_put <- (strike_price *
        exp(-interest_rate * time_to_maturity)) *
        pnorm(-delta_2) -
        (stock_price *
        pnorm(-delta_1))
    
    option_price_put <- round(option_price_put, 2)
    
    # Add call value to table 
    call_table <- cbind(call_table, option_price_call)
    
    # add put value to table
    put_table <- cbind(put_table, option_price_put)
  }
  # Format both tables as matrices
  call_table <- as.matrix(call_table)
  put_table <- as.matrix(put_table)
  
  # Transpose them
  call_table <- t(call_table)
  put_table <- t(put_table)
  
  # Give them call names
  colnames(call_table) <- c("Call")
  colnames(put_table) <- c("Put")
  
  # Merge these tables
  final_table_for_options <- data.frame(call_table,
                                        put_table)
  # Display values
  final_table_for_options
}
# Test 
delta_calculator(stock_data,
                 strike_price_1 = 3,
                 interest_rate = 0.05,
                 time_to_maturity = 1)
