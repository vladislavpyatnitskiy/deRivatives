# Calculate Confidence interval
confidence.intervals <- function(s, y, i = 0.025){
  
  i <- c(i, 1 - i) # Confidence Intervals setting up
  
  V <- apply(s, 2, function(col) sd(diff(log(s))[-1,]))*(252^.5) # Volatility
  
  r <- as.matrix(s[nrow(s),])/as.matrix(s[nrow(s)-252,]) - 1 # Expected Return
  
  A <- NULL # Empty variable for lower & upper bounds values
  
  for (j in 1:length(i)){ l <- NULL
  
    for (n in 1:ncol(s)){ m <- log(s[nrow(s),][,n]) + (r[,n] - V[n]^2 / 2) * y 
    
      l <- rbind(l, exp(m + qnorm(i[j]) * (V[n] ^ 2 * y) ^ .5)) } 
    
    A <- cbind(A, l) } # Lower & Upper Bounds
    
  colnames(A) <- c("Lower Bound", "Upper Bound") # Give column names
  
  A <- as.matrix(A) # Make it matrix
  
  rownames(A) <- colnames(s) # Give row names
  
  return(A) # Display text
}
confidence.intervals(s = stock_data, y = .25, i = .025) # Test
