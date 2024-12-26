rm(list=ls())
Data<-read.csv("F:/lessons/Multi countios Variate2/project/edited-data.csv")
#View(Data)
new.data.x<-data.frame(x1=Data$Gender,
                     x2=Data$Age,
                     x3=Data$Faculty,
                     x4=Data$Last.degree)
new.data.y<-data.frame(y1=Data$How.much.time.did.you.spend.exercising.last.week...Total.duration.of.activity.per.week.,
                       y2=Data$How.much.time.did.you.spend.last.week.on.social.and.recreational.activities..travel..excursions..etc.....Total.duration.of.,
                       y3=Data$How.much.time.did.you.spend.on.art.activities.last.week...Total.duration.of.activity.per.week.,
                       y4=Data$How.much.time.did.you.spend.on.audio.visual.activities..TV..radio..cinema..etc...last.week...Total.duration.of.activity.per.we,
                       y5=Data$How.much.time.did.you.spend.on.virtual.social.activities.last.week...Total.duration.of.activity.per.week.,
                       y6=Data$How.much.time.did.you.spend.studying.outside.of.school.last.week...Total.duration.of.activity.per.week.
                       )
#View(new.data.y)
head(new.data.y,5)
dim(new.data.y)
cor(new.data.y)
cov(new.data.y)
eigen(cor(new.data.y))
####PCA Method:

pc.r<-princomp(new.data.y , cor = TRUE , scores = TRUE)
pc.c<-princomp(new.data.y , cor = FALSE , scores = TRUE)
summary(pc.r)
pc.r$loadings
head(pc.r$scores, 10)
#like as above we have for pca method with covariance matrix.
library(factoextra)

fviz_eig(pc.r)

plot(pc.r$scores[,1],pc.r$scores[,2]
     ,xlab = "Comp1" , ylab="Comp2" ,col="Blue")
abline(h=0 , col="orange")
abline(v=0 , col="orange")
fviz_pca_ind(pc.r,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #Avoid text overlapping

fviz_pca_biplot(pc.r, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" # Individuals color
)


#### FActor Analysis
#with Correlation Matrix
fa1<-factanal(new.data.y  , 3 ,scores = "regression" 
              , rotation = "none", cor="pearson")
fa2<-factanal(new.data.y  , 3 ,scores = "Bartlett", cor="pearson")
fa3<-factanal(new.data.y  , 3 ,scores = "regression" 
              , rotation = "varimax", cor="pearson")

#ploting:
#ploting for fa1:

#windows(10,10)
par(mfrow=c(1,2))
plot(loadings(fa1)[,1],loadings(fa1)[,2],pch=16,xlab="Factor 1",
     ylab="Factor 2",col="red")
abline(h=0 , col="blue")
abline(v=0 , col="blue")
plot(loadings(fa1)[,1],loadings(fa1)[,3],pch=16,xlab="Factor 1",
     ylab="Factor 3",col="black")
abline(h=0 , col="blue")
abline(v=0 , col="blue")


#ploting for fa2:
#windows(10,10)
par(mfrow=c(1,2))
plot(loadings(fa2)[,1],loadings(fa2)[,2],pch=16,xlab="Factor 1",
     ylab="Factor 2",col="red")
abline(h=0 , col="blue")
abline(v=0 , col="blue")
plot(loadings(fa2)[,1],loadings(fa2)[,3],pch=16,xlab="Factor 1",
     ylab="Factor 3",col="black")
abline(h=0 , col="blue")
abline(v=0 , col="blue")


#ploting for fa3:
#windows(10,10)
par(mfrow=c(1,2))
plot(loadings(fa3)[,1],loadings(fa3)[,2],pch=16,xlab="Factor 1",
     ylab="Factor 2",col="red")
abline(h=0 , col="blue")
abline(v=0 , col="blue")
plot(loadings(fa3)[,1],loadings(fa3)[,3],pch=16,xlab="Factor 1",
     ylab="Factor 3",col="black")
abline(h=0 , col="blue")
abline(v=0 , col="blue")


######
#or another way to factor anayisis: 
#install.packages("psych")
library(psych)
library(ggplot2)

#with covariance matrix and varimax method:

fa4 <- fa(new.data.y,nfactors = 6, rotate = "varimax" ,
          scores = "regression" ,covar = TRUE )
fa4
n_factors <- length(fa4$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fa4$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")
#with covariance matrix and with out varimax method:
fa5 <- fa(new.data.y,nfactors = 6, rotate = "none" ,
          scores = "regression" ,covar = TRUE )
fa5
n_factors <- length(fa5$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fa5$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

#with Correlation matrix and varimax method :
fa6 <- fa(new.data.y,nfactors = 6, rotate = "varimax",
          scores = "regression" ,covar = TRUE)
fa6
n_factors <- length(fa6$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fa6$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")
#with corralation matrix and with out varimax method:
fa7 <- fa(new.data.y,nfactors = 6, rotate = "none" ,
          scores = "regression" ,covar = TRUE)
fa7
n_factors <- length(fa7$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fa7$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")





########







## Ordinary Least Squares

The first line added a column to the f data frame that consists of 51 copies of the number 1.\newline
The function \textcolor{blue}{as.matrix} converted the reordered data frame into a matrix X.\newline
The next line computed $$X'X$$, using the function \textcolor{blue}{t to get the transpose of a matrix}, and %*% for matrix multiply.\newline
                              The solve function returns the inverse of is argument; it is also used to solve linear equations if the function has two arguments.
                              
                              
                              ## Ordinary Least Squares
                              
                              The estimates and other regression summaries can be be computed, based on these sufficient statistics:\newline
                              ```{r , echo=TRUE}
                              xty <- t(X) %*% f$Fuel
                              betahat <- xtxinv %*% xty
                              betahat
                              ```
                              
                              
                              ## Ordinary Least Squares
                              \tiny
                              
                              As with simple regression the function \textcolor{blue}{lm} is used to automate the fitting of a multiple linear regression mean function. The only difference between the simple and \textcolor{pink}{multiple regression} is the formula:\newline
                              ```{r , echo=TRUE}
                              m1 <- lm(formula = Fuel ~ Tax + Dlic + Income + logMiles,
                                       data = f)
                              summary(m1)
                              ```
                              \normalsize
                              
                              ## Predictions, Fitted Values and Linear Combinations
                              \label{3.6}
                              
                              Predictions and fitted values for multiple regression use the predict command, just as for simple linear regression. If you want predictions for new data, you must specify values for all the terms in the mean function, apart from the intercept, so for example.\newline
                              ```{r , echo=TRUE}
                              predict(m1, newdata=data.frame(
                                Tax=c(20, 35), Dlic=c(909, 943), Income=c(16.3, 16.8),
                                logMiles=c(15, 17)))
                              ```
                              
                              will produce two predictions of future values for the values of the regressors indicated.\newline
                              other functions:\newline
                              For these problems you will mostly be working with added-variable plots, outlined earlier, and working with the lm function to do ols fitting. The helper functions predict get predictions, residuals get the residuals, and confint compute confidence intervals for coefficient estimates.
