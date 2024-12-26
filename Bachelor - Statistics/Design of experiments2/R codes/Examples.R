#Example 1.5
#2 ^ 2   
response<-c(210,240,180,200)
A<-factor(c(-1,1,-1,1))
B<-factor(c(-1,-1,1,1))
data=data.frame(response,A,B)
fit<-lm(response~A+B+A:B ,data = data)
anova(fit)
summary(fit)

#Example1.1
response<-c(4,6,3,7,2,-2,-4,-6)
A<-factor(c(0,0,0,0,1,1,1,1))
B<-factor(c(0,0,1,1,0,0,1,1))
data=data.frame(response,A,B)
data
fit<-lm(response~A*B , data = data)
anova(fit)[2]
summary(fit)
#Example 2.1
response<-c(2,-3,5,-2,0,-6,-3,-3,
            1,1,4,9,-2,2,-1,-1,
            0,1,0,-6,-7,-6,0,-4,
            3,8,2,0,-1,0,-2,-4)
A<-factor(c(rep(-1,16),rep(1,16)))
B<-factor(rep(c(rep(-1,8),rep(1,8)),2))
C<-factor(rep(c(rep(-1,4),rep(1,4)),4))
data=data.frame(response,A,B,C)
data
fit2<-lm(response~A*B*C,data=data )
anova(fit2)



#Example 3.1
response<-c(22,31,25,
            32,43,29,
            35,34,50,
            55,47,46,
            44,45,38,
            40,37,36,
            60,50,54,
            39,41,47)
A<-factor(c(rep(c(-1,1),each=3,4)))
B<-factor(c(rep(c(-1,1),each=6 ,2)))
C<-factor(c(rep(c(-1,1),each=12)))
data=data.frame(response,A,B,C)
data
fit3<-lm(response~A*B*C , data=data)
(a=anova(fit3))
(SST=colSums(a)[2])
coef(fit3)
fit4<-lm(response~B+C+A:C ,data = data)
summary(fit4)
anova(fit4)
predict(fit4, newdata = data.frame(A=factor(-1),B=factor(-1),C=factor(-1)) ,
        interval = "prediction")
###
p<-intersect(intersect(which(data[,2]==-1),which(data[,3]==-1)) ,which(data[,4]==-1))
residuals(fit4)[p]
###
predict(fit4, newdata = data.frame(A=factor(+1),B=factor(-1),C=factor(-1)) ,
        interval = "prediction")
p2<-intersect(intersect(which(data[,2]==+1),which(data[,3]==-1)) ,which(data[,4]==-1))
residuals(fit4)[p2]
###
predict(fit4, newdata = data.frame(A=factor(-1),B=factor(+1),C=factor(+1)) ,
        interval = "prediction")
p3<-intersect(intersect(which(data[,2]==-1),which(data[,3]==+1)) ,which(data[,4]==+1))
residuals(fit4)[p3]

###
a=sort(residuals(fit4))
a
q<-c(1:24)
pk<-(q-0.5)/24

e=
sum( (e - mean(e) ) ^2) / (length(e)-1)


par(mfrow=c(1,1))
plot(a,pk ,type = "b" ,col="blue" ,xlab="Number of Data"  ,ylab="Cumulative probabilty ")
abline(lm(pk~a),col="red")
#or another way:
#qqplot(1:24,pk ,col="blue" ,type="b" ,xlab="Number of Data"  ,ylab="Cumulative probabilty")




#example 4.6

response<-c(12,18,13,16,17,15,20,15,10,
            25,13,24,19,21,17,23)
A<-factor(rep(c(-1,1),8))
B<-factor(rep(c(-1,1),each=2,4))
C<-factor(rep(c(-1,1),each=4,2))
D<-factor(rep(c(-1,1),each=8))
data<-data.frame(response,A,B,C,D)
fit5<-lm(response~A*B*C*D ,data = data)
anova(fit5)
predict(fit5)
###
a=
q<-c(1:16)
pk<-(q-0.5)/15

plot(a,pk,type = "b" ,col="blue" ,xlab="Number of Data"  ,ylab="Cumulative probabilty ")
abline(lm(pk~a),col="red")

####
response = c(6.27,5.43,8.08,8.04,7.34,7.87,6.94,6.51,7.48,7.52,8.61,8.32,7.22,7.05,8.65,8.97,9.02,9.07)
V=factor(rep(c(0,1,2),each = 2,3))
T=factor(rep(c(0,1,2),each=6))
data=data.frame(response ,V,T)
data
fit6 = lm(response~V*T)
anova(fit6)
