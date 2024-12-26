data<-read.csv("F://lessons//Multi Countios Variate1//pdf//TABLE 3.5 diabet.csv")
#View(data)
#library("car")
#now we want to do the Exercise for chemical groups:
chemical<-which(data[,7]=="chemical")
Data.chemical<-tibble::as.tibble(data[chemical,2:6])
head(Data.chemical)
fit.chemical<-lm(cbind(relative.weight,fasting.plasma.glucose)~
                   glucose.intolerance^2+
                   insulin.resistance^2+
                   insulin.response^2+
                   glucose.intolerance:insulin.resistance+
                   glucose.intolerance:insulin.response+
                   insulin.resistance:insulin.response ,
                 data = Data.chemical)
fit.chemical
chemical.coef<-linearHypothesis(fit.chemical,hypothesis.matrix = c
                               ("glucose.intolerance=0",
                                  "insulin.resistance=0",
                                 "insulin.response=0",
                                  "glucose.intolerance:insulin.resistance=0",
                                  "glucose.intolerance:insulin.response=0",
                                  "insulin.resistance:insulin.response=0"))
summary(chemical.coef)
nd.chemical.coef<-data.frame(glucose.intolerance=c(413),
               insulin.resistance=c(344),
               insulin.response=c(270))
predict.lm(fit.chemical,newdata =nd.chemical.coef,interval="confidence")
nd.chemical.pred<-data.frame(glucose.intolerance=c(493),
                          insulin.resistance=c(288),
                          insulin.response=c(208))
predict.lm(fit.chemical,newdata =nd.chemical.pred,interval="prediction")

E=chemical.coef$SSPE
H<-chemical.coef$SSPH
lambda<-det(E)/det(E+H)
s<-min(2,3)
(R2<-1-lambda)
(A2<-1-lambda^s)
(R2roy<-eigen(solve(E)%*%H)$values[1] / (1 +eigen(solve(E)%*%H)$values[1]))
Us<-sum(eigen(solve(E)%*%H)$values)
(Alh<-(Us/s)/(1+(Us/s)))
(Apillai =sum(eigen(solve(E)%*%H)$values/(1+eigen(solve(E)%*%H)$values)))


#now we want to do the Exercise for normal groups:
normal<-which(data[,7]=="normal")
Data.normal<-tibble::as.tibble(data[normal,2:6])
head(Data.normal)
fit.normal<-lm(cbind(relative.weight,fasting.plasma.glucose)~
                   glucose.intolerance^2+
                   insulin.resistance^2+
                   insulin.response^2+
                   glucose.intolerance:insulin.resistance+
                   glucose.intolerance:insulin.response+
                   insulin.resistance:insulin.response ,
                 data = Data.normal)
summary(fit.normal)
normal.coef<-linearHypothesis(fit.normal,hypothesis.matrix = c
                                ("glucose.intolerance=0",
                                  "insulin.resistance=0",
                                  "insulin.response=0",
                                  "glucose.intolerance:insulin.resistance=0",
                                  "glucose.intolerance:insulin.response=0",
                                  "insulin.resistance:insulin.response=0"))
normal.coef
nd.normal.coef<-data.frame(glucose.intolerance=c(306),
                        insulin.resistance=c(178),
                        insulin.response=c(66))
predict.lm(fit.normal,newdata =nd.normal.coef,interval="confidence")
nd.normal.pred<-data.frame(glucose.intolerance=c(349),
                          insulin.resistance=c(172),
                          insulin.response=c(114))
predict.lm(fit.normal,newdata =nd.normal.pred,interval="prediction")

E=normal.coef$SSPE
H<-normal.coef$SSPH
lambda<-det(E)/det(E+H)
s<-min(2,3)
(R2<-1-lambda)
(A2<-1-lambda^s)
(R2roy<-eigen(solve(E)%*%H)$values[1] / (1 +eigen(solve(E)%*%H)$values[1]))
Us<-sum(eigen(solve(E)%*%H)$values)
(Alh<-(Us/s)/(1+(Us/s)))
(Apillai =sum(eigen(solve(E)%*%H)$values/(1+eigen(solve(E)%*%H)$values)))

#now we want to do the Exercise for overt groups:
overt<-which(data[,7]=="overt")
Data.overt<-tibble::as.tibble(data[overt,2:6])
head(Data.overt)
fit.overt<-lm(cbind(relative.weight,fasting.plasma.glucose)~
                   glucose.intolerance^2+
                   insulin.resistance^2+
                   insulin.response^2+
                   glucose.intolerance:insulin.resistance+
                   glucose.intolerance:insulin.response+
                   insulin.resistance:insulin.response ,
                 data = Data.overt)
summary(fit.overt)
overt.coef<-linearHypothesis(fit.overt,hypothesis.matrix = c
                                ("glucose.intolerance=0",
                                  "insulin.resistance=0",
                                  "insulin.response=0",
                                  "glucose.intolerance:insulin.resistance=0",
                                  "glucose.intolerance:insulin.response=0",
                                  "insulin.resistance:insulin.response=0"))
overt.coef
nd.overt.coef<-data.frame(glucose.intolerance=c(849),
                        insulin.resistance=c(159),
                        insulin.response=c(310))
predict.lm(fit.overt,newdata =nd.overt.coef,interval="confidence")
nd.overt.pred<-data.frame(glucose.intolerance=c(1043),
                          insulin.resistance=c(106),
                          insulin.response=c(318))
predict.lm(fit.overt,newdata =nd.overt.pred,interval="prediction")

E=overt.coef$SSPE
H<-overt.coef$SSPH
lambda<-det(E)/det(E+H)
s<-min(2,3)
(R2<-1-lambda)
(A2<-1-lambda^s)
(R2roy<-eigen(solve(E)%*%H)$values[1] / (1 +eigen(solve(E)%*%H)$values[1]))
Us<-sum(eigen(solve(E)%*%H)$values)
(Alh<-(Us/s)/(1+(Us/s)))
(Apillai =sum(eigen(solve(E)%*%H)$values/(1+eigen(solve(E)%*%H)$values)))