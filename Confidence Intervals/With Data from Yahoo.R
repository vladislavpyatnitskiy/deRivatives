# Calculate Lower & Upper Bounds for stocks
prices.confidence.intervals <- function(tickers, y = NULL, s = NULL, e = NULL,
                              i.values = c(0.025, 0.975)){
  
  p <- NULL # Set variable to contain tickers
  
  # Loop for data extraction
  for (A in tickers){ if (is.null(s) && is.null(e) && is.null(y)){
      
      # When both start date and end date are defined
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) 
      
    } else if (is.null(s) && is.null(e)) { # When time to maturity is typed
      
      p <- cbind(p,getSymbols(A,from=as.Date(Sys.Date()) - round(y*365), 
                              to=Sys.Date(), src="yahoo", auto.assign=F)[,4]) 
      
    } else if (is.null(y)) { # When both start date and end date are defined
      
      p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",
                            auto.assign=F)[,4]) } else {
                                            
    # When dates and time maturity are typed
    print("Please choose either dates or time to maturity.")
    break } } 
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA

  colnames(p) <- tickers # Put the tickers in data set
  
  s.p <- as.timeSeries(p) # Make it time series
  
  v <- apply(s.p, 2, function(col) sd(diff(log(s.p))[-1,]))*252^.5 # Volatility
  
  p.ci <- s.p[nrow(s.p),] # Select last price
  
  r <- as.matrix(s.p[nrow(s.p),]) / as.matrix(s.p[1,]) - 1 # Expected Return
  
  b.array <- NULL # Create empty variable for lower & upper bounds values
  
  # Prices for Lower & Upper bounds
  for (m in 1:length(i.values)){ b.list <- NULL
    
    for (n in 1:ncol(s.p)){ mean.ci <- log(p.ci[,n])+(r[,n]-v[n]^2/2)*y
      
      b.list <-rbind(b.list, exp(mean.ci+qnorm(i.values[m])*(v[n]^2*y)^.5)) } 
    
    b.array <- cbind(b.array, b.list)} # Lower & Upper Bounds
  
  colnames(b.array) <- c("Lower Bound", "Upper Bound") # Give column names
  
  b.array <- as.matrix(b.array) # Make it matrix
  
  rownames(b.array) <- tickers # Give row names
  
  return(b.array) # Display text
}
# Test
prices.confidence.intervals(tickers = c("XOM","CVX","SHEL"),y=.25,
                            i.values=c(.025,.975))
