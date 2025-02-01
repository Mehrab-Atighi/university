library(fitdistrplus)
data("danishuni")
library(dplyr)
n = nrow(danishuni)
r = 25
j = 0:9
df = data.frame(probability = 0:9)
k_max = length(which(danishuni$Loss >= quantile(danishuni$Loss , 0.95)))

for(k in 1:k_max){
  df1 = data.frame(K = round( choose((r + n - k - j) , (n-k) ) * choose((j + k - 1) , (k-1) ) / choose((r + n ) , (n) )  , 4))
  df <- bind_cols(df , df1)
  
}
df = df[,-1]
rownames(df) = c(paste0("j = " , 0:9))
colnames(df) = c(paste0("k = " , 1:k_max))
View(df)



