gamma.calculator <- function(S, K, v, y, r){ # Gamma
  
  d <- (log(S / K) + (r + v ^ 2 / 2) * y) / v / y ^ .5 # Delta
  
  exp(-d ^ 2 / 2) / (2 * pi) ^ .5 / S / v / y ^ .5 # Gamma value
}
gamma.calculator(49, 50, .2, .3846, .05) # Test
