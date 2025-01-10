library(fitdistrplus)
data("danishuni")
k = 100
u = 10.58 # or you can use this: sort(danishuni$Loss , decreasing = T)[k]
p = as.numeric(substr(names(which.min(abs(u - quantile(danishuni$Loss , probs = seq(from = 0 , to = 1 , 0.001))))) , 1 , 4))/100
r_k = sum((1 - p)^(1:(k-1))) * p
r_k
