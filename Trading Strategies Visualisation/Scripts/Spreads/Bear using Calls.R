spread.bear.call <- function(k1, k2, c1, c2){ # Bear Spread using calls
  
  if (isTRUE(k1 >= k2)){ message("Short Call can't be higher than Long Call") }
  
  else { # if strike prices are fine, let us calculate long and short calls:
    
    L <- rbind(cbind(seq(k1 - 5, k2, 1), -c1), # Long Call
               cbind(seq(k2 + 1, k2 + 5, 1), seq(k2 - 4 - k1, k2 - k1, 1) - 1))
    
    S <- rbind(cbind(seq(k1 - 5, k1, 1), c2),
               cbind(seq(k1 + 1, k2 + 5, 1), c2 - seq(1, 10, 1))) # Short Call
    
    colnames(L) <- c("Stock Price", "Profit from Long Call") # Column names         
    colnames(S) <- c("Stock Price", "Profit from Short Call")       
    
    DF <- merge(L, S, by = "Stock Price") # Merge stock and option data frames
    
    DF$`Total Payoff` <- DF[,2] + DF[,3] # Column for total pay-off from calls
    
    plot(x = DF[,1], y = DF[,4], las = 1, ylab = "Profit ($)", type = "l",
         xlab = "Stock Price ($)", ylim = c(min(DF[,2:4]), max(DF[,2:4])),
         main = "P&L from Bear Spread using Calls", lwd = 5, col = "red")
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # Grid & lines for calls
    
    for (n in 2:3){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2) }
    
    abline(h = 0) # Break Even line
    
    text(x = k1-2.5, y = 2.75, sprintf("Short Call, Strike Price of $%s", k1))
    text(x = k1-2.5, y = -0.75, sprintf("Long Call, Strike Price of $%s", k2))
    
    DF } # Display data frame
}
spread.bear.call(30, 35, 1, 3) # Test
