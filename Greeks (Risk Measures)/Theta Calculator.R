theta.calculator <- function(S, K, v, y, r){ # Theta
  
  d1 <- (log(S / K) + (r + v ^ 2 / 2) * y) / (v * y ^.5) # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  D <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Derived Delta 1
  
  C <- -((S * D * v)/(2 * y ^ .5)) - ((r * K) * exp(-r * y) * pnorm(d2)) # Call
  
  P <- -((S * D * v)/(2 * y ^ .5)) + ((r * K) * exp(-r * y) * pnorm(-d2)) # Put
  
  Th <- c(C, P) # Make Data Frame
  
  names(Th) <- c("Call", "Put") # Names
  
  Th # Display
}
theta.calculator(49, 50, .2, .3846, .05) # Test
