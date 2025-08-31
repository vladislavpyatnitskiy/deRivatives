spread.butterfly.put <- function(k1, k2, k3, c1, c2, c3){ # Butterfly of puts
  
  if (k1 >= k2 & k2 >= k3 & k1 >= k3) # Wrong Strike Prices
    
    return(message("Incorrect values for strike prices")) 
  
  # When all inputs for Strike Prices are correct:
  L1 <- rbind(cbind(seq(k1 - 5, k1 - 1, 1), 1 - seq(k2 - k1)),
              cbind(seq(k1, k3 + 5, 1), -c1)) # Long Put
    
  L2 <- rbind(cbind(seq(k1 - 5, k3 - 1, 1), c3 - seq(k3 - k1 + c3 - 5) - 4),
              cbind(seq(k3, k3 + 5, 1), -c3)) # Long Put
    
  S <- rbind(cbind(seq(k1 - 5, k2 - 1, 1), seq(k1 - 5, k2 - 1, 1) - k2 + c2),
             cbind(seq(k2, k3 + 5, 1), c2)) # Short Put
    
  g <- c("First Long Put", "Short Put", "Second Long Put", "Stock Price")
  f <- list(L1, S, L2) # Assign Column names to pay-off of Calls
  for (n in 1:3){ colnames(f[[n]]) = c(g[4], sprintf("Profit from %s",g[n]))}
    
  DF <- merge(f[[1]], f[[2]], by = g[4]) # Merge 
  DF <- merge(DF, f[[3]], by = g[4]) 
    
  DF$`Total Payoff` <- DF[,2] + 2 * DF[,3] + DF[,4] # Total Pay-off column
    
  plot(x = DF[,1], y = DF[,5], las = 1, ylab = "Profit ($)", type = "l",
       xlab = "Stock Price ($)", ylim = c(min(DF[,2:5]), max(DF[,2:5])),
       main = "P&L from Butterfly Spread using Puts", lwd = 5, col = "red")
    
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # Grid
    
  abline(h = 0) # Break Even line
    
  for (n in 2:4){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2) }
    
  text(x = k3 + 2.5, y = c2 - 0.25,
       sprintf("Short Put, Strike Price of $%s", k2)) # Short Put
  text(x = k3 + 2.5, y = 0.25 - c1,
       sprintf("Long Put, Strike Price of $%s", k1)) # First Long Put
  text(x = k3 + 2.5, y = 0.25 - c3,
       sprintf("Long Put, Strike Price of $%s", k3)) # Second Long Put
    
  DF # Display data frame of pay-offs
}
spread.butterfly.put(55, 60, 65, 5, 7, 10) # Test
