#p chart or pbar chart limits:
n=50
p=0.1
l=3
ucl=p+(l*sqrt((p*(1-p))/n))
cl=p
lcl=p-(l*sqrt((p*(1-p))/n))
if(lcl<0 | ucl>1){lcl=0;ucl=1}
print(paste("ucl=",ucl,"cl=",cl,"lcl=",lcl))

#p1,p2:
p1.hat=0.2150
p2.hat=0.1108
n1=1400
n2=1200
alpha=0.05
z=qnorm(alpha)
p.hat=((n1*p1.hat)+(n2*p2.hat))/(n1+n2)
Z0=(p1.hat-p2.hat)/sqrt(p.hat*(1-p.hat)*((1/n1)+(1/n2)))
print(paste("Z0=",Z0 ,"z alpha=",abs(z) ))
if(Z0>z){print("we reject H0 it means that: (p1 != p2)")}


#n estimate with p:
p=0.99
alpha=0.05
  n=1
while (p^n!=alpha){n=n+1}
print(paste("n=",n))

#n estimate with delta:
p1=0.01
p2=0.05
delta=p2-p1
l=3
n=((l/delta)^2)*p1*(1-p1)
print(paste("n=",n))

#n estimate with lcl>0 :
n=1
p=0.05
l=3
while (n<=((1-p)*l^2)/p){n=n+1}
print(paste("n=",n))


#the np chart limits:
n=50
p=0.1
l=3
ucl=(n*p)+(l*(sqrt(((n*p)*(1-p)))))
cl=n*p
lcl=(n*p)-(l*(sqrt(((n*p)*(1-p)))))
if(lcl<0 | ucl>1){lcl=0;ucl=1}
print(paste("ucl=",ucl,"cl=",cl,"lcl=",lcl))


#the p chart limits with diffrent n : 
ucl=c()
lcl=c()
pbar=0.05
l=3
n=c(1,2,3)
for( i in 1:length(n))
{
  ucl[i]=p+(l*sqrt((pbar*(1-pbar))/n[i]))
  cl[i]=pbar
  lcl[i]=p-(l*sqrt((pbar*(1-pbar))/n[i]))
  if(lcl[i]<0 | ucl[i]>1){lcl[i]=0;ucl[i]=1}
  print(paste("ucl=",ucl[i],"cl=",cl[i],"lcl=",lcl[i]))
}

#betha and alpha
p=0.3
p0=0.1
n=50
ucl=0.2272
lcl=0
betha=pbinom(n*ucl ,n,p)-pbinom(n*lcl , n,p)
alpha=1-pbinom(n*ucl ,n,p0)+pbinom(n*lcl , n,p0)
ARL0=1/alpha
ARL1=1/(1-betha)
print(paste("alpha=",alpha,"betha=",betha,"ARLo=",ARL0,"ARL1=",ARL1))


#c chart(possion):
c=200*0.035
l=3  
ucl=c+(l*sqrt(c))
cl=c
lcl=c-(l*sqrt(c))
print(paste("ucl=",ucl,"cl=",cl,"lcl=",lcl))


#acceptance probabilty in single sampling:
n=89
c=2
p=0.01
pbinom(c,n,p)


#AQL and LTPD :
p1=0.01
p2=0.06
alpha=0.05
betha=0.1
for(i in 2:1000){
n=i
c<-c(1:i)
while(alpha!=1-pbinom(c,n,p1)&betha!=pbinom(c,n,p2)){
  n=n+1
c=c[i]}}
print(n,c)




#AOQ:
N=10000
n=89
c=2
p=0.01
pa=pbinom(c,n,p)
AOQ=pa*(p*(N-n))/N
print(paste("AOQ%=",AOQ*100))



#ATI
n=89
c=2
p=0.01
pa=pbinom(c,n,p)
N=10000
ATI=n+(1-pa)*(N-n)
print(paste("ATI=",ATI))



#ASN:
n1=
n2=
paI=
ASN=(n1*paI)+((n1+n2)*(1-paI))
print(paste("ASN=",ASN))

#shib khat o ...:
p1=0.01
p2=0.05
alpha=0.05
betha=0.1
k=log10((p2*(1-p1))/(p1*(1-p2)))
s=log10((1-p1)/(1-p2))/k
h1=(log10((1-alpha)/betha))/k
h2=(log10((1-betha)/alpha))/k
print(paste("k=",k,"s=",s,"h1=",h1,"h2=",h2))


pai=pbinom(c1,n1,p1)
paii=pbinom(c2-d1 ,n2,p2)