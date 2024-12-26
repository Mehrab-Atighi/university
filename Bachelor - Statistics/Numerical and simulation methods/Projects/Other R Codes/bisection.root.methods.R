root.bisect <- function(f,x0,x1,tol = 1e-16,max.iter = 200)
{ 
  
  if (!is.function(f))                   { stop('f is not a function.')}
  if (!is.vector(x0, mode = 'numeric'))  { stop('x0 is not numeric.')} 
  if (!is.vector(x1, mode = 'numeric'))  { stop('x1 is not numeric.')} 
  if ( length(x0) != 1 )                 { stop('x0 must have length equal to 1.')}
  if ( length(x1) != 1 )                 { stop('x1 must have length equal to 1.')}  
  if ( tol <  1.0e-17)                   { tol = 1.0e-17} #if tol less -17 make it -17
  if ( !is.numeric(max.iter) )           { stop( 'max.iter must be numeric.')}
  if ( max.iter < 1 )                    { stop( 'max.iter must be positive.')}
  
  
  iter.count = 0
  
  a=x0
  b=x1
  i=0
  tol=tol
  if (f(a)*f(b) > 0 ) { stop('f  function do not bracket root beween a and b.')}
  options(digits=20)
  while((abs(b-a) > tol) && (i < max.iter) )  
  { fa=f(a)
  fb=f(b)
  i=i+1
  m=0.5*(a+b)
  if ((m==a) || (m==b)){ print("root and any point matches") ; break} 
  fm=f(m)    
  if(f(m)*f(b) <0)
  {a=m} 
  else
  {b=m  
  }}
  
  return( list('roots' = m, 'f.vals' = f(m), 'iter.count' = i, 'tol'=tol))
}
root.bisect(f,x0,x1)