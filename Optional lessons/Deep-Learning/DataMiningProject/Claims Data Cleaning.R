library(dplyr)
library(readxl)
library(ggplot2)
library(DBI)
library(tidyverse)
library(data.table)

#read all policies excel files with .xlsx pattern.

file.list = list.files( pattern='*.xlsx' , recursive = TRUE )
 Claims <- lapply(file.list[1:3], read_excel)
  names(Claims[[1]])[54] = "LifeLoss_Value"
  names(Claims[[2]])[54] = "PassengerLoss_Value"
  names(Claims[[3]])[54] = "FinanceLoss_Value"
  
# bind each files with row
 all_claims_df = bind_rows(Claims)

#remove some columns manualy

 df_claims  = as.data.table(
   all_claims_df[,c(17,54,56,57)]
   )
 df_claims = as.data.frame(df_claims)
names(df_claims)[1] = c("ID_Policy")
for(i in 2:4){
  df_claims[,i] = ifelse(is.na(df_claims[,i]) , 0  , df_claims[,i])
}
save(df_claims , file = "Claims_list.RData")
