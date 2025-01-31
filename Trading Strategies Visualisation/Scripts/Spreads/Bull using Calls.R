spread.bull.call <- function(k1, k2, c1, c2){ # Bull Spread using Calls
  
  if (isTRUE(k1 >= k2)){ message("Long Call cannot be higher than Short Call")}
  
  else { # If Strike Prices are OK, then calculate payoffs for calls
    
    L <- rbind(cbind(seq(k1 - 5, k1, 1), -c2),
               cbind(seq(k2 - 4, k2 + 5, 1), seq(k2-5-k1, k2+4-k1, 1) - 2))
    
    S <- rbind(cbind(seq(k1 - 5, k2, 1), c1),
               cbind(seq(k2+1, k2 + 5, 1), 1 - seq(k2 - 4 - k1, k2 - k1, 1)))
    
    colnames(L) <- c("Stock Price", "Profit from Long Call") # Column names         
    colnames(S) <- c("Stock Price", "Profit from Short Call")       
    
    DF <- merge(L, S, by = "Stock Price") # Merge stock and option data frames
    
    DF$`Total Payoff` <- DF[,2] + DF[,3] # Column of Total Pay-off
    
    plot(x = DF[,1], y = DF[,4], las = 1, ylab = "Profit ($)", type = "l",
         xlab = "Stock Price ($)", ylim = c(min(DF[,2:4]), max(DF[,2:4])),
         main = "P&L from Bull Spread using Calls", lwd = 5, col = "red")
    
    for (n in 2:3){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2) }
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # grid
    
    abline(h = 0) # Break Even line
    
    text(x = k1-2.5, y=1.25, sprintf("Short Call, Strike Price of $%s", k2))
    text(x = k1-2.5, y=-2.75, sprintf("Long Call, Strike Price of $%s", k1))
    
    DF } # Data Frame
}
spread.bull.call(30, 35, 1, 3) # Test
