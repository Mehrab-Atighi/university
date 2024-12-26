  #example 1.8 software book for 3^3 with 2 replication

  
      y=c(-35,100,4,-45,-10,-40,-40,80,31,17,55,-23,-65,-55,

      -64,20,110,-20,-39,90,-30,-55,-28,-61,15,110,54,-25,

      75,5,-60,30,-30,15,54,36,24,120,-5,-58,-44,-62,4,44,

     -31,-35,113,-55,-67,-26,-52,-30,135,4)


    A<-factor(rep(0:2,each=9,2))


    B<-factor(rep(0:2,each=3,6))


    C<-factor(rep(0:2,18))


    g<-data.frame(y,A,B,C)

  
    g

#fit model************************************

  lm.response<-lm(y~A*B*C,data=g)


#anova table**********************************

  anova(lm.response)

#test significant model***********************


  Fisher=summary(lm.response)$fstatistic[[1]]

  
   df1=summary(lm.response)$fstatistic[[2]]


   df2=summary(lm.response)$fstatistic[[3]]
 
  
   alpha=0.05


   cp=qf(1-alpha,df1,df2)


   if(Fisher>cp)print("model is significant") else print("model is  not significant")


#test significant model with p-value


    pvalue=1-pf(Fisher,df1,df2)

    
   if(pvalue<alpha)print("model is significant") else print("model is  not significant")

#***********************************************************

   R2=summary(lm.response)$r.squared


   SSR=sum(anova(lm.response)$S[1:7])

   SSE=(anova(lm.response)$S[8])

   SST=SSR+SSE

   SSR/SST


   R2adj=summary(lm.response)$adj.r.squared


#mean data for each treatment*************************


  aggregate(y ~ A + B +C ,FUN = mean, data = g)


#Simplify the model by excluding nonsignificant effects


  lm.response2<-lm(y~B+C+(A:B)+(A:C)+(B:C),data=g)

  
  anova(lm.response2)


#fitted.values=yhat**************************


  yhat=lm.response2$ fitted.values


  yhat=predict(lm.response2)


#residuals=e*********************************
 
  residual=lm.response2$ residuals

  cbind(y-yhat,residual)

#normality of residuals**********************

 #qunatile-quantile plot


  qqnorm(residual)


  qqline(residual)
 

 #Residuals plots standard chart

   par(mfrow=c(2,2))

   plot(lm.response2)

   box("outer")

 #**********************

   shapiro.test(residual)


   str(shapiro.test(residual))


   pvlaue=shapiro.test(residual))$p.value


   if(pvalue<alpha)print("residuals have normal distribution") else print("residuals have normal distribution")


  #effects interaction of AB*********************************

   
  library(ggplot2)


  intEf <- aggregate(y ~  A +B,FUN = mean, data = g)


  effects_interaction <- ggplot(intEf, aes(x = A, y = y, color = B)) +

  geom_point() + geom_line(aes(group = B))


  effects_interaction


 #***effects interaction of AC******************************************************


  intEf <- aggregate(y ~ A + C,FUN = mean, data = g)


  effects_interaction <- ggplot(intEf, aes(x = A, y = y, color = C)) +

  geom_point() + geom_line(aes(group = C))


  effects_interaction

#***effects interaction of BC******************************************************


  intEf <- aggregate(y ~ B + C,FUN = mean, data = g)


  effects_interaction <- ggplot(intEf, aes(x = B, y = y, color = C)) +

  geom_point() + geom_line(aes(group = C))


  effects_interaction

    

#########example 3 of fasl 3#########################


 y=c(
 
    1,5,7.5,0.2,3.2,6,0.2,3.5,7.2,1,0.4,

    6.5,1,3.2,5.2,1.2,3.7,7,1.7,4.5,6.7,

    0.2,3.7,7.5,0.5,3.7,6.2,
   
   1.7,4.2,7.7,0.7,3.5,6.2,0.3,3.2,6.7,

   0.5,3.5,6.2,0,4,6.5,0.5,4.2,6.8,1.2,

   4.7,7,1,4.2,6,1.7,3.7,7)


    D<-factor(rep(0:2,each=9,3))


    O<-factor(rep(0:2,each=3,6))


    C<-factor(rep(0:2,6))


    g<-data.frame(y,D,O,C)

  
    g

#fit model************************************

  lm.response<-lm(y~D*O*C,data=g)


#anova table**********************************

  anova(lm.response)

#test significant model***********************


  Fisher=summary(lm.response)$fstatistic[[1]]

  
   df1=summary(lm.response)$fstatistic[[2]]


   df2=summary(lm.response)$fstatistic[[3]]
 
  
   alpha=0.05


   cp=qf(1-alpha,df1,df2)


   if(Fisher>cp)print("model is significant") else print("model is  not significant")


#test significant model with p-value


    pvalue=1-pf(Fisher,df1,df2)

    
   if(pvalue<alpha)print("model is significant") else print("model is  not significant")

#***********************************************************

   R2=summary(lm.response)$r.squared

   R2

   SSR=sum(anova(lm.response)$S[1:7])

   SSE=(anova(lm.response)$S[8])

   SST=SSR+SSE

   SSR/SST


   R2adj=summary(lm.response)$adj.r.squared


#mean data for each treatment*************************


  aggregate(y ~ A + B +C ,FUN = mean, data = g)


#Simplify the model by excluding nonsignificant effects


  lm.response2<-lm(y~D+O+C+(D:O),data=g)

  
  anova(lm.response2)


#fitted.values=yhat**************************


  yhat=lm.response2$ fitted.values


  yhat=predict(lm.response2)


#residuals=e*********************************
 
  residual=lm.response2$ residuals

  cbind(y-yhat,residual)

#normality of residuals**********************

 #qunatile-quantile plot


  qqnorm(residual)


  qqline(residual)
 

 #Residuals plots standard chart

   par(mfrow=c(2,2))

   plot(lm.response2)

   box("outer")

 #**********************

   shapiro.test(residual)


   str(shapiro.test(residual))


   pvlaue=shapiro.test(residual))$p.value


   if(pvalue<alpha)print("residuals have normal distribution") else print("residuals have normal distribution")


  #effects interaction of DO*********************************

   
  library(ggplot2)


  intEf <- aggregate(y ~  D +O,FUN = mean, data = g)


  effects_interaction <- ggplot(intEf, aes(x = D, y = y, color = O)) +

  geom_point() + geom_line(aes(group = O))


  effects_interaction


 #***effects interaction of DC******************************************************


  intEf <- aggregate(y ~ D + C,FUN = mean, data = g)


  effects_interaction <- ggplot(intEf, aes(x = D, y = y, color = C)) +

  geom_point() + geom_line(aes(group = C))


  effects_interaction


#***effects interaction of OC******************************************************


  intEf <- aggregate(y ~ O + C,FUN = mean, data = g)


  effects_interaction <- ggplot(intEf, aes(x = O, y = y, color = C)) +

  geom_point() + geom_line(aes(group = C))


  effects_interaction

