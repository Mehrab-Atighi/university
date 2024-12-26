##############################
#NOW WE want to use the multivariate tests.
#library(tibble)
data<-read.csv("F://lessons//introductio sampling 2//project//data//edited.data.csv" , header = TRUE)
#View(data)
#attach(data)
Data<-tibble("Age"=as.numeric(Age),"Gender" = Gender ,"Field"=Field , 
             "Exercising" =as.numeric(How.much.time.did.you.spend.exercising.last.week...Total.duration.of.activity.per.week.),
             "social" =as.numeric(How.much.time.did.you.spend.last.week.on.social.and.recreational.activities..travel..excursions..etc.....Total.duration.of.),
             "art Activity"=as.numeric(How.much.time.did.you.spend.on.art.activities.last.week...Total.duration.of.activity.per.week.),
             "visual&Radio activity" = as.numeric(How.much.time.did.you.spend.on.audio.visual.activities..TV..radio..cinema..etc...last.week...Total.duration.of.activity.per.we),
             "Virtual activity"= as.numeric(How.much.time.did.you.spend.on.virtual.social.activities.last.week...Total.duration.of.activity.per.week.),
             "studing"=as.numeric(How.much.time.did.you.spend.studying.outside.of.school.last.week...Total.duration.of.activity.per.week.))
#View(Data)


fit<-manova(cbind(as.numeric(Data$Exercising),
                  as.numeric(Data$social),
                  as.numeric(Data$`art Activity`),
                  as.numeric(Data$`visual&Radio activity`),
                  as.numeric(Data$`Virtual activity`),
                  as.numeric(Data$studing))
            ~Age+factor(Field)+factor(Gender) ,data = Data)
#now we want to see the Pillai test outputs:
summary(fit,test = "Pillai")
#now we want to see the Wilks test outputs:
summary(fit,test = "Wilks")
#now we want to see the Hotelling-Lawley test outputs:
summary(fit,test = "Hotelling-Lawley")
#now we want to see the Roy test outputs:
summary(fit,test = "Roy")

#now we want to test the mean of multivariate
#for example now we want to chek and test that can we say that the mean of men and womens for our response (Exercise ,...) are equals or not?
s<-which(data$Gender=="Sir")
w<-which(data$Gender=="Miss")
#library("ICSNP")
HotellingsT2(cbind(as.numeric(Data$Exercising)[w],
             as.numeric(Data$social)[w],
             as.numeric(Data$`art Activity`)[w],
             as.numeric(Data$`visual&Radio activity`)[w],
             as.numeric(Data$`Virtual activity`)[w],
             as.numeric(Data$studing)[w]) ,mu=colMeans(Data[s,4:9]))

# for multivariate regression with respone varibales social,art activity , ...
#attach(Data)
model<-lm(cbind(Exercising,
                       social,
                       `art Activity`,
                       `visual&Radio activity`,
                       `Virtual activity`,
                       studing)~Age)
summary(model)

#produce by Mehrab Atighi.