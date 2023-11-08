Greeks.table <- function(S, K, v, y, r){ # Function to calculate all Greeks
  
  d1 <- (log(S / K) + (r + v ^ 2/2) * y) / v / y ^ .5 # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  d.d1 <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Derived Delta 1
  
  delta.df <- data.frame(d1, d2) # Delta option values
  
  # Theta option values
  theta.df <- data.frame(-(S * d.d1 * v/2/y^.5) - (r*K*exp(-r*y) * pnorm(d2)),
                         -(S * d.d1 * v/2/y^.5) + (r*K*exp(-r*y) * pnorm(-d2)))
  
  gamma.parameter <- exp(-d1^2 / 2) / (2 * pi)^.5 / S / v / y ^.5 # Gamma value
  
  gamma.df <- data.frame(gamma.parameter, gamma.parameter) # DF for matrix
  
  vega.parameter <- S * y ^ .5 * exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Vega value
  
  vega.df <- data.frame(vega.parameter, vega.parameter) # # DF for matrix
  
  # Data Frame with Rho option values
  rho.df <- data.frame(K*y*exp(-r*y) * pnorm(d2), - K*y*exp(-r*y) * pnorm(-d2))
  
  l.greek <- list(delta.df, theta.df, gamma.df, vega.df, rho.df) # Greeks list
  
  greeks.df <- NULL # Set variable & # Loop for joining rows
  
  for (n in seq(l.greek)){ names(l.greek[[n]]) <- c("Call", "Put")
  
    greeks.df <- rbind(greeks.df, l.greek[[n]]) } # Set up column names
  
  rownames(greeks.df) <- c("Delta","Theta","Gamma","Vega","Rho") # Row Names
  
  greeks.df # Display
}
Greeks.table(49, 50, .2, .3846, .05) # Test
