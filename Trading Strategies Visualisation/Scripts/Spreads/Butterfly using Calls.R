spread.butterfly.call <- function(k1, k2, k3, c1, c2, c3){ # Butterfly of Calls
  
  if (k1 >= k2 & k2 >= k3 & k1 >= k3) # If Strike Prices are mixed up
    
    return(message("Incorrect values of strike prices"))
  
  # if strike prices are fine, lets us define calls
  L1 <- rbind(cbind(seq(k1 - 5, k1, 1), -c1),
              cbind(seq(k1 + 1, k3 + 5, 1), seq(k1 - k3 + 1, k2 - k1, 1)))
    
  L2 <- rbind(cbind(seq(k1 - 5, k3, 1), -c3),
              cbind(seq(k3 + 1, k3 + 5, 1), seq(1, k3 - k2, 1) - 5))
    
  S <- rbind(cbind(seq(k1 - 5, k2, 1), c2),
             cbind(seq(k2 + 1, k3 + 5, 1), c2 - seq(1, k3 - k1, 1)))
    
  g <- c("First Long Call", "Short Call", "Second Long Call", "Stock Price")
  f <- list(L1, S, L2) # Assign Column names to pay-off of Calls
  for (n in 1:3){ colnames(f[[n]]) = c(g[4], sprintf("Profit from %s",g[n]))}
    
  DF <- merge(f[[1]], f[[2]], by = g[4]) # Merge 
  DF <- merge(DF, f[[3]], by = g[4]) 
    
  DF$`Total Payoff` <- DF[,2] + 2 * DF[,3] + DF[,4] # Total pay-off
    
  plot(x = DF[,1], y = DF[,5], las = 1, ylab = "Profit ($)", type = "l",
       xlab = "Stock Price ($)", ylim = c(min(DF[,2:5]), max(DF[,2:5])),
       main = "P&L from Butterfly Spread using Calls", lwd = 5, col = "red")
    
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # grid
    
  abline(h = 0) # Break Even line
    
  for (n in 2:4){ lines(x = DF[,1], y = DF[,n], lwd = 2, lty = 2,) }
    
  text(x = k1 - 2.5, y = c2 - 0.25,
       sprintf("Short Call, Strike Price of $%s", k2)) # Short Call
  text(x = k1 - 2.5, y = 0.25 - c1,
       sprintf("Long Call, Strike Price of $%s", k1)) # First Long Call
  text(x = k1 - 2.5, y = 0.25 - c3,
       sprintf("Long Call, Strike Price of $%s", k3)) # Second Long Call
    
  DF # Display data frame of pay-off
}
spread.butterfly.call(55, 60, 65, 10, 7, 5) # Test
