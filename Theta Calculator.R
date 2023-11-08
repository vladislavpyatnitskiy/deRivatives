theta.calculator <- function(S, K, v, y, r){ # Theta
  
  d1 <- (log(S / K) + (r + v ^ 2/2) * y) / (v * y ^.5) # Delta 1
  
  d2 <- d1 - v * y ^ .5 # Delta 2
  
  d.d1 <- exp(-d1 ^ 2 / 2) / (2 * pi) ^ .5 # Derived Delta 1
  
  theta.call <- -((S*d.d1*v)/(2*y^.5))-((r*K)*exp(-r*y)*pnorm(d2)) # Call
  
  theta.put <- -((S*d.d1*v)/(2*y^.5))+((r*K)*exp(-r*y)*pnorm(-d2)) # Put
  
  theta.df <- data.frame(theta.call, theta.put) # Make Data Frame
  
  names(theta.df) <- c("Call", "Put") # Names
  
  theta.df # Display
}
theta.calculator(49, 50, .2, .3846, .05) # Test
