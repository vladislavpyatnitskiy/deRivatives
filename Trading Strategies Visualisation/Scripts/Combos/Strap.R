combo.strap <- function(k, call, put){ # Straddle Combination of Calls
  
  C <- rbind(cbind(seq(k - 10, k, 1), -call),
             cbind(seq(k + 1, k + 10, 1), seq(10) - call)) # Long Call
  
  P <- rbind(cbind(seq(k - 10, k - 1, 1), put - seq(-5, 4, 1) - 1),
             cbind(seq(k, k + 10, 1), -put)) # Long Put
  
  colnames(C) <- c("Stock Price", "Profit from Long Call") # Column names
  colnames(P) <- c("Stock Price", "Profit from Long Put")
  
  DF <- merge(C, P, by = "Stock Price") # Merge stock and option data frames
  
  DF$`Total Payoff` <- 2 * DF[,2] + DF[,3] # Total Pay-off column
  
  plot(x = DF[,1], y = DF[,4], las = 1, ylab = "Profit ($)", type = "l",
       xlab = "Stock Price ($)", ylim = c(min(DF[,4]), max(DF[,4])), 
       main = "Profit & Loss from Strap", lwd = 5, col = "red")
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # grid
  
  abline(h = 0) # Break Even line and lines for calls below
  
  for (n in 2:3){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2) } 
  
  text(x = k + 7.5, y = 0.25 - put,
       sprintf("Long Put, Strike Price of $%s", k)) # Short Put
  text(x = k - 7.5, y = 0.25 - call,
       sprintf("2 Long Calls, Strike Price of $%s", k)) # First Long Put
  
  DF # Display data frame of pay-off
}
combo.strap(70, 1, 3) # Test
