  #Glomerular filtration rate 2 is the most important parameter for evaluating kidney function in kidney transplant recipients (GFR). It is considered that its use in the clinic is limited to GFR. What is the inulin secretion as a standard criterion (Chrysler et al. Perfusion scan and kidney function (measured as DTPA). (Reported DTPA GFR) (ml / min / 1.73 m2)
  #A- Draw a distribution diagram. 
  #B- Execute and interpret the regression model.
  #c-for a person with Predict and interpret the value of the dependent variable, 50 times the DTPA GFR.
  DTPA_GFR<-c(18,21,21,23,27,27,30,32,32,32,36,37,41,42,42,43,43,48,48,51,55,58,60,62,67,68,88)
  #the DTPA GFR unit is  (ml/min/1.73*m2)
  cystatin_C.invers<-c(0.213,0.265,0.446,0.203,0.369,0.568,0.382,0.383,0.274,0.424,0.308,0.498,0.398,0.485,0.427,0.562,0.463,0.549,0.538,0.571,0.546,0.402,0.592,0.541,0.568,0.800,0.667)
  #the 1/cystatin C(cystatin_C.invers) unit is (mg/L)
  data.frame(DTPA_GFR,cystatin_C.invers)
  #solve:
  
  
  #A)the scatter plot is:
  par(mfrow=c(2,1))
  plot(cystatin_C.invers,DTPA_GFR)
  
  #B)
  realationships.fit1<-lm(cystatin_C.invers~DTPA_GFR)
  plot(cystatin_C.invers~DTPA_GFR)
  abline(realationships.fit1,col="red")
  
  summary(realationships.fit1)
  
  #C)we should predict our Regression line equation.So first we have to get the slope and width from the origin of the line
  realationships.fit1
  f<-function(x)#x is the point we want to predict.
  {print((coef(realationships.fit1)[1])+coef(realationships.fit1)[2]*x)}
  # coef(relationships.fit1)[1] is the width of the origin of the desired line.
  #coef(relationships.fit1)[2] is the slope of the desired line. 
  x=32
  f(x)#is the our predict for 50DTPA GFR
  #We can see a direct and ascending relationship between our two variables 
  #if we want to have the ANOVA table for this question we should do this :
  anova(realationships.fit1)
  #then we can see the answer
  #To obtain the desired confidence interval, we do the following:
  confint(realationships.fit1)
  #This means that with 95% confidence our regression line slope is in the range (60.76594 and 125.41878).
  #This means that with 95% confidence from the origin of our regression line is in the range (-15.77013 and 15.30678).
  new2<-data.frame(DTPA_GFR=c(32))
  predict(realationships.fit1, newdata= new2, interval= "confidence")
  
  par(mfrow=c(1,1))
  pred.w.clim<-predict(realationships.fit1, interval = "confidence")
  pred.w.plim<-predict(realationships.fit1, interval = "prediction")
  matplot(DTPA_GFR,cbind(pred.w.plim,pred.w.clim ),type= "l", ylab= "prediction y")
  cor(DTPA_GFR,cystatin_C.invers)
  cor.test(DTPA_GFR,cystatin_C.invers , method = "pearson")
  e<-realationships.fit1$residuals
  print(e)
  s<-rstandard(realationships.fit1)
  print(s)
  t<-rstudent(realationships.fit1)
  print(t)
  y.hat<-realationships.fit1$fitted.values
  qqnorm(e)
  
  
  
  par(mfrow=c(3,2))
  plot(y.hat,s)
  abline(h=0 , col="blue")
  plot(DTPA_GFR,s)
  abline(h=0 , col="green")
  plot(y.hat,t)
  abline(h=0 , col="red")
  plot(DTPA_GFR,t)
  abline(h=0, col=45)
  plot(y.hat,e)
  abline(h=0,col="yellow")
  plot(DTPA_GFR,e)
  abline(h=0 , col="black")
  
  
  model.reg<-lm(cystatin_C.invers~DTPA_GFR)
  model.aov<-aov(cystatin_C.invers~factor(DTPA_GFR))
  anova(model.reg,model.aov)
  #the pure error value is:
  anova(model.reg,model.aov)[2,2]
  #the pure error df is :
  anova(model.reg,model.aov)[2,1]
  #the lack of fit value is:
  anova(model.reg,model.aov)[2,4]
  #the lack of fit df is:
  anova(model.reg,model.aov)[2,3]
  #for chek the answers we should do this:
  if(anova(model.reg,model.aov)[2,2]+anova(model.reg,model.aov)[2,4]==anova(model.reg,model.aov)[1,2])
  {print("its true")}
  
  
  DTPA_GFR.prim<-DTPA_GFR
  cystatin_c.invers.prim<-cystatin_C.invers
  for (i in 1:length(DTPA_GFR.prim)) {
    if(s[i]>2){
      DTPA_GFR.prim<-c(DTPA_GFR.prim[-i])
      cystatin_c.invers.prim<-cystatin_c.invers.prim[-i]
    }
    
  }
  for (i in 1:length(DTPA_GFR.prim)) {
    if(s[i]<= -2){
      DTPA_GFR<-c(DTPA_GFR.prim[-i])
      cystatin_c.invers.prim<-cystatin_c.invers.prim[-i]
    }
  }
  
  print(DTPA_GFR.prim)
  print(cystatin_c.invers.prim)
  print(length(DTPA_GFR.prim))
  print(length(cystatin_c.invers.prim))
  
  
  print(DTPA_GFR.prim)
  print(cystatin_c.invers.prim)
  plot(DTPA_GFR.prim,cystatin_c.invers.prim)
  realationships.fit2<-lm(cystatin_c.invers.prim~DTPA_GFR.prim)
  plot(cystatin_c.invers.prim~DTPA_GFR.prim)
  abline(realationships.fit2,col="red")
  #baraye moghayese plot ha darim:
  par(mfrow=c(2,1))
  plot(cystatin_C.invers~DTPA_GFR)
  abline(realationships.fit1,col="red")
  plot(cystatin_c.invers.prim~DTPA_GFR.prim)
  abline(realationships.fit2,col="blue")
  #baraye dashtn e,s,t jadid darim :
  e.prim<-realationships.fit2$residuals
  print(e.prim)
  s.prim<-rstandard(realationships.fit2)
  print(s.prim)
  t.prim<-rstudent(realationships.fit2)
  print(t.prim)
  y.hat.prim<-realationships.fit2$fitted.values
  
  par(mfrow=c(3,2))
  plot(y.hat.prim,s.prim)
  abline(h=0 , col="blue")
  plot(DTPA_GFR.prim,s.prim)
  abline(h=0 , col="green")
  plot(y.hat.prim,t.prim)
  abline(h=0 , col="red")
  plot(DTPA_GFR.prim,t.prim)
  abline(h=0, col=45)
  plot(y.hat.prim,e.prim)
  abline(h=0,col="yellow")
  plot(DTPA_GFR.prim,e.prim)
  abline(h=0 , col="black")
  
  #baraye moghayese plot e ha :
  par(mfrow=c(3,4))
  
  plot(y.hat.prim,s.prim)
  abline(h=0 , col="blue")
  plot(y.hat,s)
  abline(h=0 , col="blue")
  
  plot(DTPA_GFR.prim,s.prim)
  abline(h=0 , col="green")
  plot(DTPA_GFR,s)
  abline(h=0 , col="green")
  
  
  plot(y.hat.prim,t.prim)
  abline(h=0 , col="red")
  plot(y.hat,t)
  abline(h=0 , col="red")
  
  plot(DTPA_GFR.prim,t.prim)
  abline(h=0, col=45)
  plot(DTPA_GFR,t)
  abline(h=0, col=45)
  
  plot(y.hat.prim,e.prim)
  abline(h=0,col="yellow")
  plot(y.hat,e)
  abline(h=0,col="yellow")
  
  plot(DTPA_GFR.prim,e.prim)
  abline(h=0 , col="black")
  plot(DTPA_GFR,e)
  abline(h=0 , col="black")
  
  #baraye baresy lack of fit and pure error darim:
  
  model.reg.prim<-lm(DTPA_GFR.prim~cystatin_c.invers.prim)
  model.aov.prim<-aov(DTPA_GFR.prim~factor(cystatin_c.invers.prim))
  anova(model.reg.prim,model.aov.prim)
  #the pure error value is:
  anova(model.reg.prim,model.aov.prim)[2,2]
  #the pure error df is :
  anova(model.reg.prim,model.aov.prim)[2,1]
  #the lack of fit value is:
  anova(model.reg.prim,model.aov.prim)[2,4]
  #the lack of fit df is:
  anova(model.reg.prim,model.aov.prim)[2,3]
  #for chek the answers we should do this:
  if(anova(model.reg.prim,model.aov.prim)[2,2]+anova(model.reg.prim,model.aov.prim)[2,4]==anova(model.reg.prim,model.aov.prim)[1,2])
  {print("its true")}
  
  
  
  
