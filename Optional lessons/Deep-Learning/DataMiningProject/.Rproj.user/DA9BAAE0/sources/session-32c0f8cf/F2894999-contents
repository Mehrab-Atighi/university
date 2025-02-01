library(dplyr)
library(readxl)
library(ggplot2)
library(DBI)
library(tidyverse)
library(data.table)

##read all policies excel files with .xlsx pattern.

file.list = list.files( pattern='*.xlsx' , recursive = TRUE )
policies <- lapply(file.list[4:33], read_excel)
#
# # bind each files with row
all_policies_df = bind_rows(policies)


#remove some columns manualy

df_sodor  = as.data.table(
  all_policies_df[,-c(1,2,3,4,10,19,20,23,26,28,29,33,34,35,36,37)]
  )

df_sodor =
  df_sodor %>%
  separate(col = `سابقه مالی / جانی`, into = c("c1", "FinanceHistory","LifeHistory"), sep = ":", remove = T)
df_sodor = df_sodor[,-c(which(names(df_sodor) == "c1"))]
df_sodor$FinanceHistory = substr(df_sodor$FinanceHistory ,
                                        start = 1, stop = nchar(df_sodor$FinanceHistory)-6)
df_sodor= as.data.table(df_sodor)
save(df_sodor , file = "policies_list.RData")
load("policies_list.RData")
#find columns with low or very high variance to delete them



# ایجاد یک داده‌فریم نمونه با 40 ستون categorical
set.seed(123)
coded_dfs <- list()
categorical_columns <- names(which(
  sapply(df_sodor, is.factor) | sapply(df_sodor, is.character) == T))
# حلقه برای کددهی به هر ستون و ایجاد داده‌فریم جداگانه
for (col in categorical_columns) {
  # ایجاد ستون کد
  df_sodor[, paste0(col, "_Code") := as.numeric(factor(get(col)))]

  # ایجاد داده‌فریم جداگانه برای این ستون
  coded_dfs[[col]] <- df_sodor[, .(get(col), get(paste0(col, "_Code")))]
  coded_dfs[[col]] <- unique(df_sodor[, .(get(col), get(paste0(col, "_Code")))])
  names(coded_dfs[[col]]) <- c(col, paste0(col, "_Code"))
}

save(coded_dfs , file = "coded_dfs.RData")
load("coded_dfs.RData")
df_sodor = as.data.frame(df_sodor)
df_with_coding = df_sodor[,which(names(df_sodor) %notin% categorical_columns)]

# remove some linearity columns mannulay for example A+B-C = D
df_with_coding = df_with_coding[,-c(21,22)]
names(df_with_coding)
names(df_with_coding) = c("PolicyHolderCode" ,
                          "Duration",
                          "CarProductYear",
                          "SideCover_MR",
                          "FinanceCover_MR",
                          "AccidentCover_MR",
                          "ThirdParty_Pr",
                          "MultipleBloodMoney_Pr",
                          "ExcessLife_Pr",
                          "ExcessFinance_Pr",
                          "DriverAccident_Pr",
                          "Pension_Pr",
                          "Basis_Pr",
                          "Health_Complications",
                          "Goverment_Complications",
                          "CentralInsurance_Pension_Pr",
                          "ValueAdded_Tax",
                          "ValueAdded_Complications",
                          "Insurer_Pension_Pr",
                          "Net_Pr",
                          "ID_PolicyLuncher_Departmant",
                          "ID_PolicyLuncher_Province",
                          "ID_LunchDate",
                          "ID_PolicyHolderName",
                          "ID_MarkettingBy",
                          "ID_AutomobileType",
                          "ID_AutomobileGroup",
                          "ID_AutomobileClass",
                          "ID_AutomobuileUssage",
                          "ID_StartDate",
                          "ID_EndDate",
                          "ID_RegisterUser",
                          "ID_Policy",
                          "ID_PolicyHolderType",
                          "ID_LastInsurer",
                          "ID_AutomobileZipCode",
                          "ID_AutomobileSystem",
                          "ID_AutomobileZipCodeType",
                          "ID_Passengers_Claim",
                          "ID_Finance_Claim",
                          "ID_Life_Claim"
                          )
save(df_with_coding , file = "df_with_coding.RData")
load("df_with_coding.RData")
