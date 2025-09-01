combo.strangle <- function(k1, k2, call, put){ # Strangle Combination
  
  if (k1 >= k2) return(message("Long Put cannot be higher than Long Call"))
  
  # if strike prices are fine, let us define calls below:
  C <- rbind(cbind(seq(k1 - 10, k2, 1), -call),
             cbind(seq(k2 + 1, k2 + 10, 1), seq(10) - call)) # Long Call
    
  P <- rbind(cbind(seq(k1 - 10, k1 - 1, 1), put - seq(-5, 4, 1) - 1),
             cbind(seq(k1, k2 + 10, 1), -put)) # Long Put
    
  colnames(C) <- c("Stock Price", "Profit from Long Call") # Column names
  colnames(P) <- c("Stock Price", "Profit from Long Put")
    
  DF <- merge(C, P, by = "Stock Price") # Merge stock and option data frames
    
  DF$`Total Payoff` <- DF[,2] + DF[,3] # Total Pay-off column
    
  plot(x = DF[,1], y = DF[,4], las = 1, ylab = "Profit ($)", type = "l",
       xlab = "Stock Price ($)", ylim = c(min(DF[,4]), max(DF[,4])),
       main = "Profit & Loss from Strangle", lwd = 5, col = "red")
    
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # Grid
    
  abline(h = 0) # Break Even line
    
  for (n in 2:3){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2) } 
    
  text(x = k2 + 7.5, y = 0.25 - put,
       sprintf("Long Put, Strike Price of $%s", k1)) # Long Put
  text(x = k1 - 7.5, y = 0.25 - call,
       sprintf("Long Call, Strike Price of $%s", k2)) # Long Call
    
  DF # Pay-off
}
combo.strangle(55, 60, 1, 3) # Test
