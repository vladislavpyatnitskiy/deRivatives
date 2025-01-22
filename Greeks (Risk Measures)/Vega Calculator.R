vega.calculator <- function(S, K, v, y, r){ # Vega
  
  d <- (log(S / K) + (r + v ^ 2 / 2) * y) / v / y ^ .5 # Delta
  
  S * y ^ .5 * exp(-d ^ 2 / 2) / (2 * pi) ^ .5 # Vega value
}
vega.calculator(49, 50, .2, .3846, .05) # Test
