library(tidyverse)
a1 = c(0.4 , 0.1 , 0.1)
a2 = c(0.6, 0.1 , 0.1)


alpha_boresh = function(a , alpha)
{
  return( c(lower = a[1] - a[2] + (a[2]*alpha) , upper = a[1] + a[2] - (a[2]*alpha)) )
}

p_alpha = function(a , alpha)
{
  return(c(min = min(alpha_boresh(a , alpha)),
           max = max(alpha_boresh(a , alpha))))
}


alpha = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
mu_min = c()
mu_max = c()
for(i in 1:length(alpha)){
  mu_min[i] =  (p_alpha(a1 , alpha[i])[1] * 1) + (p_alpha(a2, alpha[i] )[1] * 0)
  mu_max[i] =  (p_alpha(a1 , alpha[i])[2] * 1) + (p_alpha(a2, alpha[i] )[2] * 0)
}
mu_table = tibble(mu_min , mu_max)
mu_table

