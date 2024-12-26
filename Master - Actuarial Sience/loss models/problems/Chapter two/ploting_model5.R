# now we want to ploting the Model 4
F_x5 = function(x){
  if((0 <= x) & (x< 50)){
    prob = 0.01 *x
  }
  if((50 <= x) &(x <75)){
    prob = (0.02 * x ) - 0.5
  }
  if(x >=75){
    prob = 1
  }
  return(prob)
}
#please enter your number (input) as x:
x = 50
F_x5(x)

#for example we have S as support or frre point:
S = seq(0 , 100 , 0.05)
w = c()
k = 1
for(i in S){
  w[k] = F_x5(i)
  k = k+1
}
plot(x = S , y = w , col = "blue" , type = "p" , main = "Cummulative Distribution function" , 
     xlab = "x value" , ylab = "CDF")

# for making a survial function of X we have:
S_x5 = function(x){
  prob = 1 - F_x5(x)
  return(prob)
}
x = 50
S = seq(0 , 100 , 0.05)
S_x5(x)
q = c()
k = 1
for(i in S){
  q[k] = S_x5(i)
  k = k+1
}
plot(x = S , y = q, col = "blue" , type = "p" , xlab = "x value" , ylab = "S(x)")


# for making a probability density function we have:
f_x5 = function(x){
  if((0 <= x) & (x< 50)){
    prob = 0.01
  }
  if((50 <= x) & (x< 75)){
    prob = 0.02
  }
  if(x>=75){
    prob = 0
  }
  return(prob)
}

x = 50
S = seq(0 , 100 , 0.5 )
f_x5(x)

e = c()
k = 1
for(i in S){
  e[k] = f_x5(i)
  k = k+1
}

plot( x = S , y = e  , xlab = "x value" , ylab = "Probability(pdf)"  , type = "p" , col = "blue")



# for plotting the hazard rate function of model 4 we have:

h_x5 = function(x){
  if((0 <= x) & (x< 50)){
    hr =(f_x5(x)/S_x5(x))
  }
  if((50 <= x) & (x< 75)){
    hr =(f_x5(x)/S_x5(x))
  }
  if(x>=75){
    hr =(f_x5(x)/S_x5(x))
  }
  return(hr)
}


x = 50
S = seq(0 ,70 , 0.05 )
h_x5(x)

t = c()
k = 1
for(i in S){
  t[k] = h_x5(i)
  k = k+1
}

plot( x = S , y = t   , xlab = "X value" , ylab = "h(x)"  ,
      type = "p" , col = "blue")

