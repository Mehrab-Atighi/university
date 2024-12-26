n = 500
u1 = runif(n , 0 , 1 )
y = floor(10 * u1) + 1
u2 = runif(n , 0 , 1)
q = 0.1
p = c(0.01 , 0.12 , 0.3 , 0.22 , 0.05 , 0.1 , 0.07 , 0.03 , 0.011 , 0.089)
(M = max(p/q))
N = 0
while( (M * u2 * q * y)  > ( p * y ) ){
N = N+1
  u1 = runif(n , 0 , 1 )
  y = floor(10* u1) + 1
  u2 = runif(n , 0 , 1)
  q = 0.1
  p = c(0.01 , 0.12 , 0.3 , 0.22 , 0.05 , 0.1 , 0.07 , 0.03 , 0.011 , 0.089)
  M = max(p/q)
}
(x = y)
(counts = table(y))
N

counts / sum(counts)

par(mfrow = c(1,3))
plot(p , type = "h", lwd = 5, col = "orange", ylab = "" , xlab = "")
plot(c(counts) , as.vector(counts / sum(counts)) , ylim = c(0,max((counts / sum(counts)))) , type = "h", lwd = 5 , col = "green" , ylab = "" , xlab = "")
plot(c(1:10) , as.vector(counts / sum(counts)) , ylim = c(0,max((counts / sum(counts)))) , type = "h" , lwd = 5 , col = 85, ylab = "" , xlab = "")

