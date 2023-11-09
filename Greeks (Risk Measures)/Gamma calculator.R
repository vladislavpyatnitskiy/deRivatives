gamma.calculator <- function(S, K, v, y, r){ # Gamma
  
  d1 <- (log(S / K) + (r + v ^ 2/2) * y) / v / y ^ .5 # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  d.d1 <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Derived Delta 1
  
  gamma.parameter <- d.d1 / S / v / y ^ .5 # Gamma value
  
  names(gamma.parameter) <- "Call & Put" # Name
  
  gamma.parameter # Display value
}
gamma.calculator(49, 50, .2, .3846, .05) # Test
