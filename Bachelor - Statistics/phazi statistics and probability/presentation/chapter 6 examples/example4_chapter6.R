p0 = 1/16
p4 = 1/16
p2 = 3/8

mu_alpha  = function(alpha){
  
  return(c(lower = 1.9 + (0.1 * alpha) , 
         upper = 2.1 - (0.1 * alpha)))
}
mu_alpha(alpha)

mu_table = tibble(mu_min = mu_alpha(alpha)[1:11] , mu_max = mu_alpha(alpha)[12:22])
mu_table
var_alpha = function(alpha){
  
  return(c(lower = 0.99 + 0.02 * alpha , 
         upper = 1))
}
var_alpha(alpha)
var_table = tibble(var_min = var_alpha(alpha)[1:11] , var_max = 1)
var_table


