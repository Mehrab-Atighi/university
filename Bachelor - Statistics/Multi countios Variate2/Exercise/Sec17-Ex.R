library(haven)
library(klaR)
library(MASS)
data = read_sav("F:/lessons/Multi Countios Variate2/data/Table 8.3 football.sav")
#View(data)
m1<-manova(cbind(data$WDIM,data$CIRCUM,data$FBEYE,data$EYEHD,
                 data$EARHD,data$JAW)~data$group)
(s_m1 = summary(m1))
E_matrix = s_m1$SS$Residuals
(err = diag(E_matrix / 87))
m2 = lda(group~. ,data = data ,method = "moment")
a1 = m2$scaling
round((a_star= sqrt(err) * a1[,1]) ,digits = 3)
#for froward method we have:
m2
(M1 = greedy.wilks(group~ . , data = data))
pp= predict(m2)
pp$x
plot(m2)






data=read.table("F:/lessons/Multi countios Variate2/data/apple-data.txt" , header = TRUE)
tail(data , 4)
library(klaR)
library(MASS)
attach(data)
m1 = greedy.wilks(group~. , data = data)
M1 = lda(group~ . ,data= data )
