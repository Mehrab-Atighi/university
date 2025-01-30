library(dplyr)
library(readxl)
library(ggplot2)
library(DBI)
library(tidyverse)
library(data.table)


load("df_with_coding.RData")
load("Claims_list.RData")
load("coded_dfs.RData")
names(coded_dfs[["شماره کامل"]])[1] = "ID_Policy"
# coded_dfs[["شماره کامل"]]$ID_Policy = as.character(coded_dfs[["شماره کامل"]]$ID_Policy)
claims_with_code = left_join(df_claims , coded_dfs[["شماره کامل"]] , by = "ID_Policy",keep = F )
claims_with_code = claims_with_code[,-c(1)] #remove ID_Ploicy column
names(claims_with_code)[4] = "ID_Policy"
FinalDf = left_join(df_with_coding , claims_with_code , by = "ID_Policy")
save(FinalDf , file = "FinalDf.RData")
