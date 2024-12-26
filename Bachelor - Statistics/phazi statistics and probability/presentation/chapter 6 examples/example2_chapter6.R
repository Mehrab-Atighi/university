a1 = c(0.2 , 0.1 , 0.1)
a2 = c(0.2 , 0.1 , 0.1)
a4 = c(0.2 , 0.01 , 0.01)
a5 = c(0.2 , 0.01 , 0.01)


alpha_boresh = function(a , alpha)
{
  return( c(lower = a[1] - a[2] + (a[2]*alpha) , upper = a[1] + a[2] - (a[2]*alpha)) )
}
alpha = c(0 , 0.04 , 0.12 , 0.2 , 0.3 , 0.43 ,
          0.48 , 0.5 , 0.54 , 0.6 , 0.68 , 0.75 ,
          0.8 , 0.85 , 0.9 , 0.93 , 0.96 , 1)

alpha_boresh_a1_lower = c()
alpha_boresh_a1_upper = c()
alpha_boresh_a2_lower = c()
alpha_boresh_a2_upper = c()
alpha_boresh_a4_lower = c()
alpha_boresh_a4_upper = c()
alpha_boresh_a5_lower = c()
alpha_boresh_a5_upper = c()
for(i in 1:length(alpha))
{
  alpha_boresh_a1_lower[i] =  alpha_boresh(a1 , alpha[i])[1]
  alpha_boresh_a2_lower[i] =  alpha_boresh(a2 , alpha[i])[1]
  alpha_boresh_a4_lower[i] =  alpha_boresh(a4 , alpha[i])[1]
  alpha_boresh_a5_lower[i] =  alpha_boresh(a5 , alpha[i])[1]
  alpha_boresh_a1_upper[i] =  alpha_boresh(a1 , alpha[i])[2]
  alpha_boresh_a2_upper[i] =  alpha_boresh(a2 , alpha[i])[2]
  alpha_boresh_a4_upper[i] =  alpha_boresh(a4 , alpha[i])[2]
  alpha_boresh_a5_upper[i] =  alpha_boresh(a5 , alpha[i])[2]
}
library(tidyverse)
alpha_boresh_table = tibble(alpha_boresh_a1_lower , alpha_boresh_a1_upper , "-",
       alpha_boresh_a2_lower , alpha_boresh_a2_upper , "--",
       alpha_boresh_a4_lower , alpha_boresh_a4_upper , "---",
       alpha_boresh_a5_lower , alpha_boresh_a5_upper)

alpha_boresh_table
#######################################################
p_alpha = function(a , alpha)
{
  return(c(max = max(alpha_boresh(a , alpha)),
  min = min(alpha_boresh(a , alpha))))
}
p_alpha(a1 , 1)
p1_alpha_max = c()
p2_alpha_max = c()
p4_alpha_max = c()
p5_alpha_max = c()
p1_alpha_min = c()
p2_alpha_min = c()
p4_alpha_min = c()
p5_alpha_min = c()
for( i in 1 : length(alpha))
{
p1_alpha_max[i] = p_alpha(a1 , alpha[i])[1]
p2_alpha_max[i] = p_alpha(a2 , alpha[i])[1]
p3_alpha = 0.2
p4_alpha_max[i] = p_alpha(a4 , alpha[i])[1]
p5_alpha_max[i] = p_alpha(a5 , alpha[i])[1]
p1_alpha_min[i] = p_alpha(a1 , alpha[i])[2]
p2_alpha_min[i] = p_alpha(a2 , alpha[i])[2]
p4_alpha_min[i] = p_alpha(a4 , alpha[i])[2]
p5_alpha_min[i] = p_alpha(a5 , alpha[i])[2]
}

p_alpha_table = tibble(p1_alpha_min , p1_alpha_max , "_",
                       p2_alpha_min , p2_alpha_max , "__",
                       p4_alpha_min , p4_alpha_max , "___",
                       p5_alpha_min , p5_alpha_max)

######################################################

Prob_A_alpha = function(alpha)
{
  
  return(prob_A_alpha = c( lower = 1 - p3_alpha - (p4_alpha_max + p5_alpha_max ) ,
                           upper = 1 - p3_alpha - (p4_alpha_min + p5_alpha_min)))
  
}

Prob_A_alpha = tibble(Prob_A_alpha_min = Prob_A_alpha(alpha)[1:18] ,Prob_A_alpha_max = Prob_A_alpha(alpha)[19:36])

#########################################

Prob_Ac_alpha_max = 1 - Prob_A_alpha(alpha)[1:18]
Prob_Ac_alpha_min = 1 - Prob_A_alpha(alpha)[19:36]

Prob_Ac_alpha = tibble(Prob_Ac_alpha_min , Prob_Ac_alpha_max)

#########################################


(Prob_A_Ac_alpha = tibble(prob_A_Ac_alpha = Prob_Ac_alpha + Prob_A_alpha))



######################

alpha_boresh_table = tibble(alpha_boresh_a1_lower , alpha_boresh_a1_upper ,
                            alpha_boresh_a2_lower , alpha_boresh_a2_upper ,
                            alpha_boresh_a4_lower , alpha_boresh_a4_upper ,
                            alpha_boresh_a5_lower , alpha_boresh_a5_upper)

p_alpha_table = tibble(p1_alpha_min , p1_alpha_max ,
                       p2_alpha_min , p2_alpha_max ,
                       p4_alpha_min , p4_alpha_max ,
                       p5_alpha_min , p5_alpha_max)

Prob_A_alpha = tibble(Prob_A_alpha_min = Prob_A_alpha(alpha)[1:18] ,
                      Prob_A_alpha_max = Prob_A_alpha(alpha)[19:36])

Prob_Ac_alpha = tibble(Prob_Ac_alpha_min , Prob_Ac_alpha_max)


Total_table = tibble(alpha_boresh_table, "|" , p_alpha_table, "||" , Prob_A_alpha, "|||" , Prob_Ac_alpha , "||||" , Prob_A_Ac_alpha)
View(Total_table)



