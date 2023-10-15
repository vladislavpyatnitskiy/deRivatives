# Calculate Lower & Upper Bounds for stocks
confidence_intrvl <- function(tickers, time_to_maturity = NULL,
                              start_date = NULL, end_date = NULL,
                              interval_values = c(0.025, 0.975)){
  
  # Set variable to contain tickers
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in tickers){
    if (is.null(start_date) && is.null(end_date) && is.null(time_to_maturity)){
      
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = start_date,
                                          to = end_date,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) 
      # When time to maturity is typed
      } else if (is.null(start_date) && is.null(end_date)) {
                                                          
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) -
                                            round(time_to_maturity * 365),
                                          to = Sys.Date(),
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) 
      # When dates are typed
      } else if (is.null(time_to_maturity)) {
                                            
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = start_date,
                                          to = end_date,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) } else {
      
      # When dates and time maturity are typed
      print("Please choose either dates or time to maturity.")
                                            break } } 
  
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(portfolioPrices) <- tickers
  
  # Make data discrete
  stock_price <- ROC(portfolioPrices, type = "discrete")
  
  # Make it time series
  stock_price <- as.timeSeries(portfolioPrices)
  
  # Log returns
  volatility <- diff(log(stock_price))[-1,]
  
  # Calculate volatility
  volatility <- apply(volatility, 2, function(col) sd(col)) * (252 ^ 0.5) 
  
  # Select last price
  price_ci <- stock_price[nrow(stock_price),]
  
  # Calculate expected return
  expected_return <- as.matrix(stock_price[nrow(stock_price),]) /
    as.matrix(stock_price[1,]) - 1
  
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
  rownames(array_bound) <- tickers
  
  # Display text
  return(array_bound)
}
# Test
confidence_intrvl(tickers = c("XOM", "CVX", "SHEL"), time_to_maturity = 0.25,
                  interval_values = c(0.025, 0.975))
