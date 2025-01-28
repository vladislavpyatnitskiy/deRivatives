options.vs.stocks <- function(q, buy, k){ # Option & Stock Pay-off Comparison
  
  D <- cbind(seq(buy - 10, buy + 5, .25), seq(buy - 30, buy - 15, .25) * q)
  
  f <- rbind(cbind(seq(buy - 10, buy - k -.5, .25), -2000),
             cbind(seq(buy - k, buy + 5, .25),
                   ((seq(buy - k, buy + 5, .25) - buy + k) - 1) * 2000))
  
  colnames(D) <- c("Stock Price", "Profit from Stocks") # Column names         
  colnames(f) <- c("Stock Price", "Profit from Options")       
    
  DF <- merge(D, f, by = "Stock Price") # Merge stock and option data frames
  
  plot(x = DF[,1], y = DF[,3], las = 1, ylab = "Profit ($)", type = "l",
       xlab = "Stock Price ($)", lwd = 3, col = "red",
       main = "P&L from two alternative strategies for speculating on a stock")
  
  lines(x = DF[,1], y = DF[,2], lwd = 2, lty = 2) # line for stock payoff
  
  legend("topleft", legend = c("Buy options", "Buy shares"), lty = c(1, 2),
         col = c("red", "black"), lwd = 2, bty = "n") # legend
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3) # grid
  
  abline(h = 0) # Break Even line
}
options.vs.stocks(100, 25, 2.5) # Test
