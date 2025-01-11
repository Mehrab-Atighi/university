# now we want to ploting the Model 3 
F_x3 = function(x){
  if(x < 0){
    prob = 0
  }
  if((x<1)&(x>=0)){
    prob = 0.5
  }
  if((x<2)&(x>=1)){
    prob = 0.75
  }
  if((x<3)&(x>=2)){
    prob = 0.87
  }
  if((x<4)&(x>=3)){
    prob = 0.95
  }
  if(x>=4){
    prob = 1
  }
  return(prob)
}
#please enter your number (input) as x:
x = 3
F_x3(x)

#for example we have S as support or frre point:
S = seq(-2 , 10 , 0.02)
w = c()
k = 1
for(i in S){
  w[k] = F_x3(i)
  k = k+1
}
 plot(x = S , y = w , col = "blue" , type = "b" , xlab = "x value" , ylab = "Probability")
barplot(w , names.arg = S , col = 4, 
        xlab = "x value" , ylab = "Probability" , main = "Cumulative Distribution function")

# for making a survial function of X we have:
S_x3 = function(x){
  prob = 1 - F_x3(x)
  return(prob)
}

x = 3
S_x3(x)
S = seq(-2 , 10,0.02)
q = c()
k = 1
for(i in S){
  q[k] = S_x3(i)
  k = k+1
}
 plot(x = S , y = q , col = "blue" , type = "b" , xlab = "x value" , ylab = "Probability")
 barplot(q , names.arg = S , col = 4, 
         xlab = "x value" , ylab = "Probability" , main = "Survial function")
 # for making a probability density function we have:
 f_x3 = function(x){
   prob = F_x3(x) - F_x3(x-1)
   return(prob)
 }
 
x = 3
f_x3(x)
S = seq(-2 , 10 ,0.02)
e = c()
k = 1
for(i in S){
  e[k] = f_x3(i)
  k = k+1
}

barplot( e  ,names.arg = -10:10 ,
         xlab = "X value" , ylab = "Probability" ,
         col = 5:1)
 

 
####


