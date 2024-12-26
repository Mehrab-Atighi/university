#Anova:
# tarhe moteadel:

timar1<-c(9,9,15,17,6)
timar2<-c(15,17,14,16,16)
timar3<-c(12,14,17,21,17)
timar4<-c(18,20,22,16,20)
timar5<-c(9,12,11,18,11)

#Yi.:
y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
(ybar1.<-mean(timar1))
(ybar2.<-mean(timar2))
(ybar3.<-mean(timar3))
(ybar4.<-mean(timar4))
ybar5.<-mean(timar5)


#Y.. , Ybar.. :
n=length(timar1)
a=5
N=n*a


y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

#SS ha:

SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum(y1.^2,y2.^2,y3.^2,y4.^2,y5.^2))/n)-((y..^2)/N)
SSE<-SST-SStimar

#MS ha:

MStimar<-SStimar/(a-1)
MSE<-SSE/(N-a)

#F0:

F0<-MStimar/MSE

# azmon Anova:
alpha<-0.01
F<-qf(1-alpha,a-1,N-a)
if(F0 > F){
  print("H0 reject")
}else{
  print("H0 accept")
}
##################
#Azmon Anova
# tarhe na moteadel:
timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5

SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar

MStimar<-SStimar/(a-1)
MSE<-SSE/(N-a)

F0<-MStimar/MSE

alpha<-0.01
F<-qf((1-alpha),a-1,N-a)
if(F0 > F){
  print("H0 reject")
}else{
  print("H0 accept")
}


#-------------------------------------------------------
#azmoon LSD:
#i , j ro bayad malom konim
# tarhe na motadel:
timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5
a=

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar

MSE<-SSE/(N-a)

#mu1 , mu2:
t0<-(ybar1.-ybar2.)/(sqrt(MSE*((1/n1)-(1/n2))))
alpha= 0.01
if(abs(t0) > qt(1-alpha/2 , N-a)){
  print("H0  reject")
}else{
  print("H0 accept")
}
#########
#Azmoon LSD:
#tarhe moteadel:
timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

n=length(timar1)  
a=5
N=n*a

SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n , (y2.^2)/n , (y3.^2)/n , (y4.^2)/n ,(y5.^2)/n )))-((y..^2)/N)
SSE<-SST-SStimar
#mu1 , mu2:
alpha= 0.05
LSD<-qt(1-alpha/2,N-a) * (sqrt(MSE*((1/n)-(1/n))))

if((abs(ybar1.-ybar2.) > LSD)==TRUE){
  print("H0  reject")
}else{
  print("H0 accept")
}

#-----------------------------------------------------------
#Azmoon danet:
#i , j ro bayad malom konim
# tarhe na motadel:
timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5
a=

  y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N  
    
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar

MSE<-SSE/(N-a)

t2<-(ybar1.-ybar2.)/(sqrt(MSE*((1/n1)-(1/n2))))
t3<-(ybar1.-ybar3.)/(sqrt(MSE*((1/n1)-(1/n3))))
t4<-(ybar1.-ybar4.)/(sqrt(MSE*((1/n1)-(1/n4))))
t5<-(ybar1.-ybar5.)/(sqrt(MSE*((1/n1)-(1/n5))))

alpha= 0.01
d=3.95
if(abs(t2) > d){
  print("H0  reject(t2)")
}else{
  print("H0 accept(t2)")
}
if(abs(t3) > d){
  print("H0  reject(t3)")
}else{
  print("H0 accept(t3)")
}
if(abs(t4) > d){
  print("H0  reject(t4)")
}else{
  print("H0 accept(t4)")
}
if(abs(t5) > d){
  print("H0  reject(t5)")
}else{
  print("H0 accept(t5)")
}
#----------------------------------------------------------
#danken azmoon:
timar1<-c(9,9,15,17,6)
timar2<-c(15,17,14,16,16)
timar3<-c(12,14,17,21,17)
timar4<-c(18,20,22,16,20)
timar5<-c(9,12,11,18,11)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

a=5
alpha=0.05
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar

MSE<-SSE/(N-a)

nh=a/sum(1/n1,1/n2,1/n3,1/n4,1/n5)
Sybari.=sqrt(MSE/nh)

r2=2.95
r3=3.10 
r4=3.18
r5=3.25 
  

(R2=r2*Sybari.)
(R3=r3*Sybari.)
(R4=r4*Sybari.)
(R5=r5*Sybari.)
  
(ybar3.-ybar1.)
(ybar3.-ybar5.)
(ybar3.-ybar2.)




  
#------------------------------------------------------------
#azmon newoman kolz:
timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

a=5
alpha=0.05
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar

MSE<-SSE/(N-a)

nh=a/sum(1/n1,1/n2,1/n3,1/n4,1/n5)
Sybari.=sqrt(MSE/nh)
q2=   
q3=
q4=  
q5=  
  
K2=q2*Sybari.
K3=q3*Sybari.
K4=q4*Sybari.
K5=q5*Sybari.



#---------------------------------------------------------
#Azmoon Toki:

timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

a=5
alpha=0.05
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar

MSE<-SSE/(N-a)

nh=a/sum(1/n1,1/n2,1/n3,1/n4,1/n5)
Sybari.=sqrt(MSE/nh)
q=
Talpha=q*Sybari.

if(ybar1.-ybar2.>Talpha){
  print("ekhtelaf mani dare")
}else{
  print("ekhtelaf mani dar nist!!!")
}
#------------------------------------------------------------
#moghabele ha:

timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

c1=
c2=
c3=
c4=
c5=

C=sum(c1*y1. , c2*y2. , c3*y3. , c4*y4. , c5*y5)

SSmoghabele=(C^2)/sum(n1*(c1^2) , n2*(c2^2) , n3*(c3^2) , n4*(c4^2) , n5*(c5^2))
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar
MSE<-SSE/(N-a)
alpha=

F0<-SSmoghabele/MSE

if(F0>qf(1-alpha,N-a)){
  print("H0 reject")
}else{
  print("H0 accept")
}
#-------------------------------------------------------------
#Azmoon shefe:

timar1<-c(12,10,11,14)
timar2<-c(21,19,20,23)
timar3<-c(22,25,24,25)
timar4<-c(27,26,25,27)
timar5<-c(29,26,25,27)

n1=length(timar1)  
n2=length(timar2)  
n3=length(timar3) 
n4=length(timar4)  
n5=length(timar5)
N=n1+n2+n3+n4+n5

y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)

c11=
c21=
c31=
c41=
c51=
  
  C1=sum(c11*y1. , c21*y2. , c31*y3. , c41*y4. , c51*y5)
SSC1=(C1^2)/sum(n1*(c11^2) , n2*(c21^2) , n3*(c31^2) , n4*(c41^2) , n5*(c51^2))
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar
MSE<-SSE/(N-a)
alpha=
  
  F0<-SSC1/(MSE*(a-1))

if(F0>qf(1-alpha,N-a)){
  print("H0 reject")
}else{
  print("H0 accept")
}


c12=
c22=
c32=
c42=
c52=
  
  C1=sum(c11*y1. , c21*y2. , c31*y3. , c41*y4. , c51*y5)
SSC1=(C1^2)/sum(n1*(c11^2) , n2*(c21^2) , n3*(c31^2) , n4*(c41^2) , n5*(c51^2))
SST<-(sum(timar1^2,timar2^2,timar3^2,timar4^2,timar5^2)-((y..^2)/N))
SStimar<-((sum( (y1.^2)/n1 , (y2.^2)/n2 , (y3.^2)/n3 , (y4.^2)/n4 ,(y5.^2)/n5 )))-((y..^2)/N)
SSE<-SST-SStimar
MSE<-SSE/(N-a)
alpha=
  
  F0<-SSC1/(MSE*(a-1))

if(F0>qf(1-alpha,N-a)){
  print("H0 reject")
}else{
  print("H0 accept")
}



#modele tasadofi:::::

timar1<-c(9,9,15,17,6)
timar2<-c(15,17,14,16,16)
timar3<-c(12,14,17,21,17)
timar4<-c(18,20,22,16,20)
timar5<-c(9,12,11,18,11)

#Yi.:
y1.<-sum(timar1)
y2.<-sum(timar2)
y3.<-sum(timar3)
y4.<-sum(timar4)
y5.<-sum(timar5)

#Ybar i.:
ybar1.<-mean(timar1)
ybar2.<-mean(timar2)
ybar3.<-mean(timar3)
ybar4.<-mean(timar4)
ybar5.<-mean(timar5)


#Y.. , Ybar.. :
n=length(timar1)
a=5
N=n*a
alpha=

y..<-sum(timar5,timar4,timar3,timar2,timar1)
ybar..<-y../N

SStimar<-n*(sum(ybar1.^2 , ybar2.^2 , ybar3.^2 , ybar4.^2 , ybar5.^2) - (a*(ybar..^2)))
SST<-sum(timar5^2,timar4^2,timar3^2,timar2^2,timar1^2)
SSE<-SST-SStimar


MStimar<-SStimar/(a-1)
MSE<-SSE/(N-a)

#HO==>  simga to == 0    H1==> sigma to > 0
F0<-MSE/MStimar
if(F0>qf(1-alpha,a-1,N-a)){
  print("H0 reject")
}else{
  print("H0 accept")
}



#confidence interval:

#for mu_1  when we have sigma:
sigma=
alpha=
n1=
L=ybar1.-(qnorm(1-alpha/2)*(sigma/sqrt(n1)))
U=ybar1.+(qnorm(1-alpha/2)*(sigma/sqrt(n1)))

#for mu_1 when we dont have sigma:
alpha=
N=
a=
L=ybar1.-(qt(1-alpha/2,N-a)*(sqrt(MSE/n1)))
U=ybar1.+(qt(1-alpha/2,N-a)*(sqrt(MSE/n1)))


#for sigma^2  
N=
a=
alpha=
L=((N-a)*MSE)/qchisq(alpha/2,N-a)
U=((N-a)*MSE)/qchisq(1-alpha/2,N-a)


#for sigma to ^2 / simga2 + sigma to ^2 
alpha=0.05
N=25
a=5
L=(1/n)*(MStimar/MSE)*(1/qf(alpha/2 , a-1 , N-a))
U=(1/n)*(MStimar/MSE)*(1/qf(1-alpha/2 , a-1 , N-a))


#----------------------------------------------------------------
rm(list=c())

q<-c(timar1-mean(timar1),timar2-mean(timar2),timar3-mean(timar3),timar4-mean(timar4),timar5-mean(timar5))
e<-sort(q)

p<-c()
for(k in 1:25){
  p[k]=(k-0.5)/25
}

plot(p,e,col="BLUE",main="emtehan")


c<-c(1,-4,6,-4,1)
o<-c(56,78,81,96,61)
sum(o*c)


qf(0.95,1,20)
