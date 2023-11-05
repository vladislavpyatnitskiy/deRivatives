# Function to calculate option value via generalisation
gen.one.step.bin.tree <- function(y, s, d, r){
  
  bin <- (exp(r * y) - (s - d) / s) / (2 * d / s) # Calculate probability
  
  sprintf("Option Value today is %s", round(exp(-r*y) * (bin + 0*(1 - bin)),3))
}
gen.one.step.bin.tree(y = .25, s = 20, d = 2, r = .12) # Test
