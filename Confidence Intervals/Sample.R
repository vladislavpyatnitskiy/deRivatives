# Function to calculate confidence intervals to stock volatility
confidence_interval <- function(p, r, v, y, R = 95){
  
  MEAN <- log(p) + (r - v ^ 2 / 2) * y # Mean
  SD <- (v ^ 2 * y) ^ .5 # Standard Deviation
  
  L <- exp(MEAN + qnorm((100 - R) / 200) * SD) # Lower Bound
  U <- exp(MEAN + qnorm(R * .01 + (100 - R) / 200) * SD) # Upper Bound
  
  sprintf("%s%% probability stock price will be between %.2f and %.2f.",R,L,U) 
}
confidence_interval(p = 40, r = .16, v = .2, y = .5, 99) # Test
