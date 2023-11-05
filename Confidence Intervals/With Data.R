# Calculate Confidence interval
confidence.intervals <- function(s, y, i.values=c(0.025, 0.975)){
  
  v <- apply(s, 2, function(col) sd(diff(log(s))[-1,]))*(252^.5) # Volatility
  
  r <- as.matrix(s[nrow(s),])/as.matrix(s[nrow(s)-252,]) - 1 # Expected Return
  
  b.array <- NULL # Empty variable for lower & upper bounds values
  
  # Create empty variable to contain values for lower & upper bounds
  for (m in 1:length(i.values)){ b.list <- NULL
    
    for (n in 1:ncol(s)){ mean.ci <- log(s[nrow(s),][,n]) + (r[,n]-v[n]^2/2)*y 
      
      b.list <- rbind(b.list,exp(mean.ci + qnorm(i.values[m])*(v[n]^2*y)^.5))} 
    
    b.array <- cbind(b.array, b.list) } # Lower & Upper Bounds
  
  colnames(b.array) <- c("Lower Bound", "Upper Bound") # Give column names
  
  b.array <- as.matrix(b.array) # Make it matrix
  
  rownames(b.array) <- colnames(s) # Give row names
  
  return(b.array) # Display text
}
confidence.intervals(s=stock_data,y=.25,i.values=c(.025,.975)) # Test
