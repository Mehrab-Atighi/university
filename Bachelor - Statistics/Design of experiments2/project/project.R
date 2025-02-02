# the first question for lesson project :
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
fit1<-lm(response~A*B*C , data=data)

#anova table**********************************

anova(fit1)

#test significant model***********************


(Fisher=summary(fit1)$fstatistic[[1]])


(df1=summary(fit1)$fstatistic[[2]])


(df2=summary(fit1)$fstatistic[[3]])


alpha=0.05


(cp=qf(1-alpha,df1,df2))


if(Fisher>cp)print("model is significant") else print("model is  not significant")


#test significant model with p-value


(pvalue=1-pf(Fisher,df1,df2))


if(pvalue<alpha)print("model is significant") else print("model is  not significant")

#***********************************************************

(R2=summary(fit1)$r.squared)


(SSR=sum(anova(fit1)$S[1:7]))

(SSE=(anova(fit1)$S[8]))

(SST=SSR+SSE)

(SSR/SST)


(R2adj=summary(fit1)$adj.r.squared)


#mean data for each treatment*************************


aggregate(response ~ A + B +C ,FUN = mean, data = data)


#Simplify the model by excluding nonsignificant effects


fit2<-lm(response~B+C+(A:C),data=data)


anova(fit2)


#fitted.values=yhat**************************


(yhat=fit2$ fitted.values)


(yhat=predict(fit2))


#residuals=e*********************************

(residual=fit2$ residuals)

cbind(response - yhat,residual)

#normality of residuals**********************

#qunatile-quantile plot


qqnorm(residual)


qqline(residual)


#Residuals plots standard chart

par(mfrow=c(2,2))

plot(fit2)

box("outer")

#**********************
#cheking normality of residuals with shapiro test:

shapiro.test(residual)


str(shapiro.test(residual))


(pvlaue=shapiro.test(residual)$p.value)


if(pvalue<alpha)print("residuals have normal distribution") else print("residuals have normal distribution")


#effects interaction of AB*********************************


library(ggplot2)


intEf <- aggregate(response ~  A +B,FUN = mean, data = data)


effects_interaction <- ggplot(intEf, aes(x = A, y = response, color = B)) +
  
  geom_point() + geom_line(aes(group = B))


effects_interaction


#***effects interaction of AC******************************************************


intEf <- aggregate(response ~ A + C,FUN = mean, data = data)


effects_interaction <- ggplot(intEf, aes(x = A, y = response, color = C)) +
  
  geom_point() + geom_line(aes(group = C))


effects_interaction

#***effects interaction of BC******************************************************


intEf <- aggregate(response ~ B + C,FUN = mean, data = data)


effects_interaction <- ggplot(intEf, aes(x = B, y = response, color = C)) +
  
  geom_point() + geom_line(aes(group = C))


effects_interaction



#the second question for lesson project:
response <- c(11,9,10,10,11,10,8,9,
              15,10,16,14,12,9,6,15,
              9,12,11,11,11,11,11,12,
              16,17,15,12,13,13,11,11,
              10,11,15,8,6,8,9,14,
              12,13,14,13,9,13,14,9,
              10,12,13,10,7,7,17,13,
              15,12,15,13,12,12,9,14)
A = factor(rep(rep(c(-1,+1) , each = 8),4))
B = factor(rep(rep(c(-1,-1,+1,+1), each = 8) ,2))
C = factor(rep(c(-1,+1),each = 32))

data2 = data.frame(Response = response , A = A , B = B , C = C)

fit3 = lm(response ~ A*B*C ,data = data2)

#anova table**********************************

anova(fit3)

#test significant model***********************


(Fisher=summary(fit3)$fstatistic[[1]])


(df1=summary(fit3)$fstatistic[[2]])


(df2=summary(fit3)$fstatistic[[3]])


alpha=0.05


(cp=qf(1-alpha,df1,df2))


if(Fisher>cp)print("model is significant") else print("model is  not significant")


#test significant model with p-value


(pvalue=1-pf(Fisher,df1,df2))


if(pvalue<alpha)print("model is significant") else print("model is  not significant")

#***********************************************************

(R2=summary(fit3)$r.squared)


(SSR=sum(anova(fit3)$S[1:7]))

(SSE=(anova(fit3)$S[8]))

(SST=SSR+SSE)

(SSR/SST)


(R2adj=summary(fit3)$adj.r.squared)


#mean data for each treatment*************************


aggregate(response ~ A + B +C ,FUN = mean, data = data2)


#Simplify the model by excluding nonsignificant effects


fit4<-lm(response~A,data=data2)


anova(fit4)


#fitted.values=yhat**************************


(yhat=fit4$ fitted.values)


(yhat=predict(fit4))


#residuals=e*********************************

(residual=fit4$ residuals)

cbind(response - yhat,residual)

#normality of residuals**********************

#qunatile-quantile plot


qqnorm(residual)


qqline(residual)


#Residuals plots standard chart

par(mfrow=c(2,2))

plot(fit4)

box("outer")

#**********************
#cheking normality of residuals with shapiro test:

shapiro.test(residual)


str(shapiro.test(residual))


(pvlaue=shapiro.test(residual)$p.value)


if(pvalue<alpha)print("residuals have normal distribution") else print("residuals have normal distribution")


#effects interaction of AB*********************************


library(ggplot2)


intEf <- aggregate(response ~  A +B,FUN = mean, data = data2)


effects_interaction <- ggplot(intEf, aes(x = A, y = response, color = B)) +
  
  geom_point() + geom_line(aes(group = B))


effects_interaction


#***effects interaction of AC******************************************************


intEf <- aggregate(response ~ A + C,FUN = mean, data = data2)


effects_interaction <- ggplot(intEf, aes(x = A, y = response, color = C)) +
  
  geom_point() + geom_line(aes(group = C))


effects_interaction

#***effects interaction of BC******************************************************


intEf <- aggregate(response ~ B + C,FUN = mean, data = data2)


effects_interaction <- ggplot(intEf, aes(x = B, y = response, color = C)) +
  
  geom_point() + geom_line(aes(group = C))


effects_interaction

