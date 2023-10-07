delta_calculator <- function(x,
                             strike_price_1,
                             interest_rate,
                             time_to_maturity){
  
  # Define tables for calls and puts
  call_table <- NULL
  put_table <- NULL
  
  # For each column
  for (n in 1:ncol(x)){ security <- x[,n]
    
    # Extract recent observations
    stock_price <- security[nrow(x),]
    
    # Calculate strike price
    strike_price <- strike_price_1 + stock_price
    
    # Calculate returns
    rtrns_delta <- diff(log(security))[-1,]
    
    # Calculate standard deviation
    sd_for_delta <- apply(rtrns_delta, 2, function(col) sd(rtrns_delta))
    
    # Calculate first and second deltas  
    delta_1 <- (log(stock_price/strike_price) +
                  (interest_rate + ((sd_for_delta^2)/2)) * time_to_maturity) /
      (sd_for_delta * (time_to_maturity)^0.5)
    
    delta_2 <- delta_1 - (sd_for_delta * (time_to_maturity^0.5))
    
    # Calculate option call & put prices
    option_price_call <- stock_price * pnorm(delta_1) - strike_price *
      exp(-interest_rate * time_to_maturity) * pnorm(delta_2)
    
    option_price_put <- (strike_price*exp(-interest_rate*time_to_maturity)) *
        pnorm(-delta_2) - (stock_price * pnorm(-delta_1))
    
    # Round to 2 decimals
    option_price_call <- round(option_price_call, 2)
    option_price_put <- round(option_price_put, 2)
    
    # Add call and put values to table 
    call_table <- rbind(call_table, option_price_call)
    put_table <- rbind(put_table, option_price_put) }
  
  # Format both tables as matrices
  call_table <- as.matrix(call_table)
  put_table <- as.matrix(put_table)
  
  # Merge these tables
  final_table_for_options <- data.frame(call_table, put_table)
  
  # Assign column names
  colnames(final_table_for_options) <- c("Call", "Put")
  
  # Assign row names
  rownames(final_table_for_options) <- colnames(x)
  
  # Display values
  final_table_for_options
}
# Test 
delta_calculator(stock_data, 
                 strike_price_1 = 3,
                 interest_rate = 0.055,
                 time_to_maturity = 1)
