install.packages("numDeriv")
f<-function(x){3*exp(x)-1/x}
newton.raphson <- function(f, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) 
  
x0 <-0.25
k <- 3
a=0.2
b=0.3
  
fa <- f(a)
if (fa == 0.0) {
    return(a)
  }
  
fb <- f(b)
if (fb == 0.0) {
    return(b)
  }
  
for (i in 1:n) {
dx <- genD(func = f, x = x0)$D[1] 
x1 <- x0 - (f(x0) / dx) 
k[i] <- x1 
  
if (abs(x1 - x0) < tol) {
root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
   
    x0 <- x1
  }
}
newton.raphson(f,2,3)
