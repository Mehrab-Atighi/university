f_x = function(x){
  prob = 4 * x * (1 + x^2)^-3
  return(prob)
}
r =c()
S = seq(0, 3 , 0.00001)
k = 1
for(i in S){
  r[k] = f_x(i)
  k = k+1
}

plot(x = S , y = r , xlab = "x value" , ylab = "pdf")

(Mode = S[which.max(r)])
  