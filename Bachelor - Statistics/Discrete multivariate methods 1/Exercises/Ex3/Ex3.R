#q3- chapter 3:
library(MASS)
library(nnet)
Sex = factor(c(rep(c("Male" , "Female") , c(128,22)),
               rep(c("Male" , "Female") , c(125,23))))
Darman = factor(rep(c("Motevali" , "Dore-b-Dore"),c(150,148)))
response = factor(c(rep(c("pishrafte_bimari" ,"bedone_taghir" , "behbode_andak" , "behbode_kamel") , c(28,45,29,26)),
                    rep(c("pishrafte_bimari" ,"bedone_taghir" , "behbode_andak" , "behbode_kamel") , c(3,12,5,2)),
                    rep(c("pishrafte_bimari" ,"bedone_taghir" , "behbode_andak" , "behbode_kamel") , c(41,44,20,20)),
                    rep(c("pishrafte_bimari" ,"bedone_taghir" , "behbode_andak" , "behbode_kamel") , c(12,7,3,1))))

table(response , Sex , Darman)

m1 <- polr(response ~ Sex + Darman ,
           Hess = TRUE , method = "logistic")
summary(m1)

m2 <- polr(response ~ Sex * Darman ,
           Hess = TRUE , method = "logistic")
summary(m2)



#q3- chapter 4:
library(nnet)

hezb <- factor(c(rep(c("demokrat" , "jomhori_khah" , "mostaghel") , c(132,176,127)) , 
                rep(c("demokrat" , "jomhori_khah" , "mostaghel") , c(42,6,12)) ,
                rep(c("demokrat" , "jomhori_khah" , "mostaghel") , c(172,129,130)) , 
                rep(c("demokrat" , "jomhori_khah" , "mostaghel") , c(56,4,15))))
race = factor(c(rep(c("white" , "black"),c(435 , 60)) ,
                rep(c("white" , "black"),c(431 , 75))))
Sex <- factor(rep(c("Male" , "Female") , c(495 , 506)))


df <- data.frame(hezb , race , Sex)
df$hezb = relevel(df$hezb , ref ="jomhori_khah")
levels(df$hezb)

m1 <- multinom(hezb ~ . , data = df )
summary(m1 , correlation = FALSE , Wald = TRUE)


#q8- chapter 5:


