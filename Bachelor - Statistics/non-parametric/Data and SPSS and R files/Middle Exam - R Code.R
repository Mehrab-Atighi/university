# generate new numbers :
n=
u= runif(n , min = 0 , max = 1)

x<-function(u){
  return(  )
}

x(u)

####################################
# antegrale amare tartibi:
r=6
n=13
p=1/5
I= 1 - pbinom(r-1 , n , p)
I
f=((factorial(r-1)*factorial(n-r))/factorial(n))
f
J= f*I
J

###############################
# Amare tartibi:
n=4
r=
V_r.mean = function (r,n){
  mean.v_r = r/(n+1)}
V_r.mean(r,n)

V_r.var = function(r,n){
 return (var.v_rv = (r*(n-r+1))/(((n+1)^2) * (n+2)))}
V_r.var(r,n)


n=4
i=1
j=4
if ( i>j){
  Cov.V_i.V_j =  (j*(n-i+1))/(((n+1)^2) * (n+2))
}else {
  Cov.V_i.V_j = (i*(n-j+1))/(((n+1)^2) * (n+2))
}
Cov.V_i.V_j 



rho = Cov.V_i.V_j /sqrt(V_r.var(i,n)*V_r.var(j,n))
rho

#########################################################################

f<- function(x) {
  f=2*x
}
solve(integrate(f,0,Q)==p)

####################################
# confidence interval Gamma for  Q_ p   (Y-J , y_K )
n=12
p=0.25
j=6
k=7
gamma = pbinom(k-1,n,p) - pbinom(j-1,n,p)
gamma


#nonparametrics confidence interval for Q-p
x<-c(4.2,7,8.2,4.6,11,6.6,9.2,9,7.8,7.6,8.3,8.4)
p=0.5
n=10
s=0.967
x.bar=3.72
alpha=0.01
t=qt(1-alpha,n-1)

L= x.bar - (s/sqrt(n))*t
L
U= x.bar + (s/sqrt(n))*t
U



# normal approxmatly:
alpha=0.05
n=16
p=0.75
q=1-p
j = (n*p) + 0.5 - (qnorm(1-alpha)*sqrt(n*p*q))
j
k= (n*p) + 0.5 + (qnorm(1-alpha)*sqrt(n*p*q))
k

gamma = pbinom(round(k-1),n,p) - pbinom(round(j-1),n,p)
gamma




# azmoon neshane : 
# 1 taile :
data=c(172,184,170,150,164,171,172,177,151,190)
a=155
b=length(which(data > a))
n=length(data)-length(which(data == a))
p=0.25
#H0 : Q-p = a
#H1 : Q-p > a
#C : {b>=k}

p_value = 1 - pbinom(b-1 , n , 1-p)
p_value
alpha=0.05
if(p_value < alpha){
  print("H0 reject")
}else{
  print("H0 accept")
}


########1 tailed another side

# 1 taile :
data=c(64,55,58,63,74,70)
a=63
b=length(which(data > a))
n=length(data)-length(which(data == a))
p=0.8
#H0 : Q-p = a
#H1 : Q-p < a
#C : {b>=k}

p_value =  pbinom(b , n , 1-p)
p_value
alpha=0.05
if(p_value < alpha){
  print("H0 reject")
}else{
  print("H0 accept")
}



##################
# 2 tail :
data=c(172,184,170,150,164,171,172,177,151)
a=155
b=length(which(data > a))
n=length(data)-length(which(data == a))
p=0.25
#H0 : Q-p = a
#H1 : Q-p =! a
#C : {b>=k} + {b<= k}

p_value =  2*min(pbinom(b , n , 1-p),1-pbinom(b-1,n,1-p))
p_value
alpha= 0.1
if(p_value < alpha){
  print("H0 reject")
}else{
  print("H0 accept")
}

