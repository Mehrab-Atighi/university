library(dplyr)
library(readxl)
library(ggplot2)
library(DBI)
library(tidyverse)
library(data.table)

load("FinalDf.RData")
Profit_data = FinalDf
for(i in 42:44){
  Profit_data[,i] = ifelse(is.na(Profit_data[,i]) , 0  , Profit_data[,i])
}
Profit_data$GrossPremium =  Profit_data$Net_Pr+ Profit_data$Goverment_Complications +
  Profit_data$ValueAdded_Tax + Profit_data$ValueAdded_Complications - Profit_data$CentralInsurance_Pension_Pr 
Profit_data$TotalLoss = Profit_data$LifeLoss_Value +
  Profit_data$PassengerLoss_Value +
  Profit_data$FinanceLoss_Value
Profit_data$Profit = Profit_data$GrossPremium - Profit_data$TotalLoss

#remove other value columns
Profit_data = Profit_data[,-c(7:20)]
save(Profit_data , file = "Profit_data.RData")
