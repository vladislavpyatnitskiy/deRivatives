# Function to calculate option value
one.step.binomial.tree <- function(y, s, d, r, o){ 

  sprintf("Value of option today is %s",round((s-(s-d)*exp(-r*y))/2/d*o,3)) 
}
one.step.binomial.tree(y = 0.25, s = 20, d = 2, r = 0.12, o = 1) # Test
