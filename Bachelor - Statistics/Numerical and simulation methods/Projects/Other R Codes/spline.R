x<-c(1,3,5,7,8)
y<-c(1,9,25,49,64)
q<-matrix(splines::interpSpline(x,y)$coef , nrow = length(x/2))
x_0=6
#for interpolate between (x[1],x[2]) we have:
f_1_2<-function(x_0)
{
  q[1,1]+q[1,2]*(x_0-x[1])^1/factorial(1)+q[1,3]*(x_0-x[1])^2/factorial(2)+q[1,4]*(x_0-x[1])^3/factorial(3)
}
#for interpolate between (x[2],x[3]) we have:

f_2_3<-function(x_0)
{
 q[2,1]+q[2,2]*(x_0-x[2])^1/factorial(1)+q[2,3]*(x_0-x[2])^2/factorial(2)+q[2,4]*(x_0-x[2])^3/factorial(3)
}
#for interpolate between (x[3],x[4]) we have:

f_3_4<-function(x_0)
{
  q[3,1]+q[3,2]*(x_0-x[3])^1/factorial(1)+q[3,3]*(x_0-x[3])^2/factorial(2)+q[3,4]*(x_0-x[3])^3/factorial(3)
}

if(x[1]<=x_0&x_0<=x[2]){
  print(f_1_2(x_0))
}
if(x[2]<x_0&x_0<=x[3]){
  print(f_2_3(x_0))
}
if(x[3]<x_0&x_0<=x[4]){
  print(f_3_4(x_0))
}
if(x_0>x[4]){
  print("this point is not in our confidence")
}