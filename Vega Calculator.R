vega.calculator <- function(S, K, v, y, r){ # Vega Option
  
  d1 <- (log(S / K) + (r + v ^ 2/2) * y) / v / y ^ .5 # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  d.d1 <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Derived Delta 1
  
  vega.parameter <- S * y ^ .5 * d.d1 # Vega value
  
  vega.parameter # Display value
}
vega.calculator(49, 50, .2, .3846, .05) # Test
