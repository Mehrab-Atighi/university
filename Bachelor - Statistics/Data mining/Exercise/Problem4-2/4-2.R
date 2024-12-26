rm(list=ls())
Data<-read.csv("F:/lessons/Data mining/Data/Universities.csv")
#View(Data)
head(Data)
dim(Data)


#a)

#Now we want to delete two categorical varibale from our data 
data <- Data[,-c(2,3)]
chek_na<- is.na(data)
n=1
index<-c()
for( i in 1:nrow(chek_na)){
  if(sum(chek_na[i,])>=1){
    index[n]=i
    n=n+1
  }
}

data<-data[-index,]
dim(data)



#b)

pca<-prcomp(data[,-1] ,scale. = TRUE ,center = TRUE)
summary(pca)
head(pca$rotation,2)
head(pca$x, 2)


#install.packages(factoextra)
library(factoextra)
fviz_eig(pca)#plot(pca)



plot(pca$x[,1],pca$x[,2]
     ,xlab = "Comp1" , ylab="Comp2" ,col="Blue")

#biplot(pca)
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)      #Avoid text overlapping)

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )
