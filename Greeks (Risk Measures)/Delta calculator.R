delta.calculator <- function(x, k, r, y){
  
  O <- NULL
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
  
    s.p <- as.numeric(s[nrow(x),]) # Extract recent observations
    
    k.p <- k + s.p # Calculate strike price
    
    v <- apply(s, 2, function(col) sd(diff(log(s))[-1,])) # standard deviation
    
    D <- (log(s.p / k.p) + (r + v ^ 2 / 2) * y) / (v * y ^ .5)
    D2 <- D - v * y ^ .5 # Calculate first and second deltas
    
    C <- round(s.p * pnorm(D) - k.p * exp(-r * y) * pnorm(D2), 2) # Call 
    P <- round(k.p * exp(-r * y) * pnorm(-D2) - s.p * pnorm(-D), 2) # Put
    
    O <- rbind.data.frame(O, cbind(C, P)) } # Join
  
  colnames(O) <- c("Call", "Put") # Assign column names
  
  O # Display values
}
delta.calculator(stock_data, k = 3,r = 0.055, y = 1) # Test
