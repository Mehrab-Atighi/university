mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mean.sample=c()
median.sample=c()
mode.sample=c()


for(i in 1:10000){
  y=rnorm(1000,0,1)
  mean.sample[i]=mean(y)
  median.sample[i]=median(y)
  mode.sample[i]=mode(y)
}

c( Mean = mean(mean.sample) , Median = mean(median.sample)  , Mode = mean(mode.sample))
c( Mean.var = var(mean.sample) , median.var = var(median.sample) , mode.var = var(mode.sample))


par(mfrow = c(1 , 3))
plot(cumsum(mean.sample)/1:length(mean.sample) , col = "blue")
abline(h = 0.01 , col = "red")
plot(cumsum(median.sample)/1:length(median.sample) , col = "blue")
abline(h = 0.01 , col = "red")
plot(cumsum(mode.sample)/1:length(mode.sample) , col = "blue")
abline(h = 0.01 , col = "red")


