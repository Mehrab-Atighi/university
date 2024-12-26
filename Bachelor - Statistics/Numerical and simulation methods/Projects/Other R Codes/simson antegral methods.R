composite.simpson <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / n
  
  xj <- seq.int(a, b, length.out = n + 1)
  xj <- xj[-1]
  xj <- xj[-length(xj)]
  
  approx <- (h / 3) * (f(a) + 2 * sum(f(xj[seq.int(2, length(xj), 2)])) + 4 * sum(f(xj[seq.int(1, length(xj), 2)])) + f(b))
  
  return(approx)
  
}
f<- function(x)
{return(sqrt(x))}
composite.simpson(f ,1,1.3 ,6)




