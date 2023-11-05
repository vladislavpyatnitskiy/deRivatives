delta.calculator <- function(x, k, r, y){
  
  call.t <- NULL # Call table
  put.t <- NULL # Put table
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
  
    s.p <- s[nrow(x),] # Extract recent observations
    
    k.p <- k + s.p # Calculate strike price
    
    d.sd <- apply(s, 2, function(col) sd(diff(log(s))[-1,])) # standard deviation
    
    d.1 <- (log(s.p / k.p) + (r + d.sd ^ 2/2) * y) / (d.sd * y ^ .5)
    d.2 <- d.1 - d.sd * y ^ .5 # Calculate first and second deltas
    
    # Add call and put values to table 
    call.t <- rbind(call.t, round(s.p*pnorm(d.1) - k.p*exp(-r*y)*pnorm(d.2),2))
    put.t <- rbind(put.t, round(k.p*exp(-r*y)*pnorm(-d.2) - s.p*pnorm(-d.1),2)) }
    
  options.t <- data.frame(as.matrix(call.t), as.matrix(put.t)) # Merge tables
    
  colnames(options.t) <- c("Call", "Put") # Assign column names
  rownames(options.t) <- colnames(x) # Assign row names
    
  options.t # Display values
}
delta.calculator(stock_data, k = 3,r = 0.055, y = 1) # Test
