library(MVTests)
result<-Mpaired(T1=bumpus[1:21,],T2=bumpus[21:49,])

y1<-c(81,461,20,450,246,166,63,64,155,151,16,37,223,138,72,245)
y2<-c(74,423,16,450,87,115,50,50,113,38,156,27,218,138,39,231)
x1<-c(72,134,84,98,48,142,113,90,30,260,116,87,669,100,315,188)
x2<-c(33,18,20,58,13,49,38,24,18,34,20,27,32,27,39,65)
data<-data.frame(y1,y2,x1,x2)
View(data)
#a)
#H0:mu_d = 0
#H1:mu_d != 0
# dij = yij - xij   , mu_di = di  for j = 1,2,...,16,i = 1,2

#b)
result<-Mpaired(T1=data[,1:2],T2=data[,3:4])
summary(result)

#c)
#for x1,y1: 
#H0:mu_d = 0
#H1:mu_d != 0
# di = y1j - x1j   for j = 1,2,...,16
t.test(x=data[,1],data[,3],paired= TRUE)
#for x1,y1: 
#H0:mu_d = 0
#H1:mu_d != 0
# di = y2j - x2j   for j = 1,2,...,16
t.test(x=data[,2],data[,4],paired= TRUE)






