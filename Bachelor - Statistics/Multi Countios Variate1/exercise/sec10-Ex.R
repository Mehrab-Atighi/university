#a)
a<-cbind(c(4,-2,1,-3))
mu<-cbind(c(-2,3,-1,5))
#mean
t(a)%*%mu

#Variance 
sigma<-matrix(c(11,-8,3,9,-8,9,-3,-6,3,-3,2,3,9,-6,3,9) , nrow= 4  ,byrow = TRUE)

t(a) %*%  sigma %*% a




#b)

A<-matrix(c(3,-1,2,1,-3,2,-4,1,4,-1,-2,-5) ,ncol=4  , byrow = FALSE)
#mean
A%*% mu

#variance

A %*% sigma %*% t(A)
