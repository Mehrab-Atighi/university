  # now we want to ploting the Model 4
  F_x4 = function(x){
    if(x < 0){
      prob = 0
    }
    if(x >= 0){
      prob = 1 - 0.3 * exp(-0.00001 * x)
    }
    return(prob)
  }
  #please enter your number (input) as x:
  x = 3
  F_x4(0)
  
  #for example we have S as support or frre point:
  S = seq(-10 , 10^5 , 0.5)
  w = c()
  k = 1
  for(i in S){
    w[k] = F_x4(i)
    k = k+1
  }
  plot(x = S , y = w , col = "blue" , type = "p")
  
  # for making a survial function of X we have:
  S_x4 = function(x){
    prob = 1 - F_x4(x)
    return(prob)
  }
  x = 3
  S = seq(-10 , 10^5 , 0.5)
  S_x4(x)
  q = c()
  k = 1
  for(i in S){
    q[k] = S_x4(i)
    k = k+1
  }
  plot(x = S , y = q,
       col = "blue" , type = "p" ,
       xlab = "x value" , ylab = "Probability")
  
  
  # for making a probability density function we have:
  f_x4 = function(x){
    if(x == 0){
      prob = 0.7
    }
    if(x > 0){
      prob = 0.000003 * exp(-0.00001 * x)
    }
    return(prob)
  }
  
  x = 3
  S = seq(0 , 10^5 , 0.5 )
  f_x4(x)
  
  e = c()
  k = 1
  for(i in S){
    e[k] = f_x4(i)
    k = k+1
  }
  
  plot( e   ,
        xlab = "X value" , ylab = "Probability"  ,
        type = "p")
  
  
  
  # for plotting the hazard rate function of model 4 we have:
  
  S = seq(0 , 10^5 , 0.5)
  h = 0.00001
  plot(S ,  xlim = c(0 , 10^8) , ylim = c(0,0.0001),
       col = "blue" , type = "n" ,
       xlab = "x value" , ylab = "h(x)")
  abline(a = h , b = 0 , col = "blue")
  text(x = 10^7 , y = 0.00002,labels = "h(x) = 0.00001")
  