Greeks.table <- function(S, K, v, y, r){ # Function to calculate all Greeks
  
  d1 <- (log(S / K) + (r + v ^ 2 / 2) * y) / v / y ^ .5 # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  D <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Derived Delta 1
  
  DELTA <- data.frame(d1, d2) # Delta option values
  
  # Theta option values
  THETA <- data.frame(-(S * D * v/2/y^.5) - (r * K * exp(-r * y) * pnorm(d2)),
                      -(S * D * v/2/y^.5) + (r * K * exp(-r * y) * pnorm(-d2)))
  
  G <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 / S / v / y ^.5 # Gamma value
  
  GAMMA <- data.frame(G, G) # DF for matrix
  
  V <- S * y ^ .5 * exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Vega value
  
  VEGA <- data.frame(V, V) # # DF for matrix
  
  # Data Frame with Rho option values
  RHO <- data.frame(K*y*exp(-r*y) * pnorm(d2), - K*y*exp(-r*y) * pnorm(-d2))
  
  letters <- list(DELTA, THETA, GAMMA, VEGA, RHO) # Greeks list
  
  GREEKS <- NULL # Set variable & # Loop for joining rows
  
  for (n in seq(letters)){ names(letters[[n]]) <- c("Call", "Put")
  
    GREEKS <- rbind(GREEKS, letters[[n]]) } # Set up column names
  
  rownames(GREEKS) <- c("Delta", "Theta", "Gamma", "Vega", "Rho") # Row Names
  
  GREEKS # Display
}
Greeks.table(49, 50, .2, .3846, .05) # Test
