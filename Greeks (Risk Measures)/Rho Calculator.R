rho.calculator <- function(S, K, v, y, r){ # Rho Option
  
  d2 <- (log(S / K) + (r + v ^ 2 / 2) * y) / v / y ^ .5 - v * y ^ .5 # Delta
  
  R <- c(K * y * exp(-r * y) * pnorm(d2), -K * y * exp(-r * y) * pnorm(-d2))
  
  names(R) <- c("Call", "Put") # Rho names
  
  R # Display value
}
rho.calculator(49, 50, .2, .3846, .05) # Test
