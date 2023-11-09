rho.calculator <- function(S, K, v, y, r){ # Rho Option
  
  d1 <- (log(S / K) + (r + v ^ 2/2) * y) / v / y ^ .5 # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  # Data Frame with Rho option values
  rho.df <- data.frame(K*y*exp(-r*y) * pnorm(d2), - K*y*exp(-r*y) * pnorm(-d2))
  
  names(rho.df) <- c("Call", "Put") # Rho names
  
  rho.df # Display value
}
rho.calculator(49, 50, .2, .3846, .05) # Test
