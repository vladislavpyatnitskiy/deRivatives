# Function to calculate confidence intervals to stock volatility
confidence_interval <- function(p, r, v, y, rg = 95){
  
  mean.ci <- log(p) + (r - v ^ 2 / 2) * y # Mean
  
  sd.ci <- (v ^ 2 * y) ^ .5 # Standard Deviation
  
  lower_bound <- exp(mean.ci + qnorm((100 - rg) / 200) * sd.ci) # Lower Bound
  
  upper_bound <- exp(mean.ci+qnorm(rg*.01 + (100-rg)/200)*sd.ci) # Upper Bound
  
  sprintf("%s%% probability stock price will be between %.2f and %.2f.",
          rg, lower_bound, upper_bound) # Values in text
}
confidence_interval(p = 40, r = .16, v = .2, y = .5, 99) # Test
