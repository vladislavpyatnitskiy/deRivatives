spread.bear.put <- function(k1, k2, c1, c2){ # Bear Spread using Puts
  
  if (k1 >= k2) return(message("Short put cannot be higher than Long Put"))
  
  # if strike prices are fine, let us define long and short puts:
  L <- rbind(cbind(seq(k1 - 5, k2, 1), k1 + c2 - c1 - seq(k1 - 5, k2, 1)),
             cbind(seq(k2 + 1, k2 + 5, 1), -c2)) # Long Put
    
  S <- rbind(cbind(seq(k1 - 5, k1 - 1, 1), seq(k1 - 5, k1 - 1, 1) - k1 + c1),
             cbind(seq(k1, k2 + 5, 1), c1)) # Short Put
    
  colnames(L) <- c("Stock Price", "Profit from Long Put") # Column names         
  colnames(S) <- c("Stock Price", "Profit from Short Put")       
    
  DF <- merge(L, S, by = "Stock Price") # Merge stock and option data frames
    
  DF$`Total Payoff` <- DF[,2] + DF[,3] # Calculate total pay-off from puts
    
  plot(x = DF[,1], y = DF[,4], las = 1, ylab = "Profit ($)", type = "l",
       xlab = "Stock Price ($)", ylim = c(min(DF[,2:4]), max(DF[,2:4])),
       main = "P&L from Bear Spread using Puts", lwd = 5, col = "red")
    
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # Grid and lines for puts
    
  for (n in 2:3){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2) }
    
  abline(h = 0) # Break Even line
    
  text(x = k2 + 2.5, y = 1.25, sprintf("Short Put, Strike Price of $%s", k1))
  text(x = k2 + 2.5, y = -2.75, sprintf("Long Put, Strike Price of $%s", k2))
    
  DF # display data frame
}
spread.bear.put(30, 35, 1, 3) # Test
