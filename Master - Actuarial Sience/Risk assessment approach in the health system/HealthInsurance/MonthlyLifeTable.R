
###importing annual Life table
annual_life_table = readxl::read_xlsx("Life_Table.xlsx")

#Calculating death number and death and lives probabilities:
for(i in 1:nrow(annual_life_table)){
  annual_life_table$dx[i] = annual_life_table$Lx[i] - annual_life_table$Lx[i+1]
  annual_life_table$Px[i] = annual_life_table$Lx[i+1] / annual_life_table$Lx[i]
  annual_life_table$qx[i] = 1 - annual_life_table$Px[i]
}

#making Monthly life table
monthly_life_table = data.frame(
  year = rep(0:106 , each = 12),
  Month = rep(1:12 , 107),
  Age = cumsum(rep(1/12 , nrow(annual_life_table) * 12)) ,
  Lxm = 1 ,
  dxm = 0 ,
  px = 0,
  qx = 0 )
monthly_life_table$Lxm[1] = 99927.416667
for(j in 1:(nrow(monthly_life_table)-1)  ){
  monthly_life_table$Lxm[j+1] = monthly_life_table$Lxm[j] - (annual_life_table$dx[(floor((j)/12))+1] / 12)
}
monthly_life_table$Lxm = round(monthly_life_table$Lxm,5)
for(j in 1:(nrow(monthly_life_table))){
  monthly_life_table$dxm[j] = monthly_life_table$Lxm[j] - monthly_life_table$Lxm[j+1]
}
monthly_life_table$dxm = round(monthly_life_table$dxm,5)
for(j in 1:(nrow(monthly_life_table)-1)){
  monthly_life_table$px[j] = monthly_life_table$Lxm[j+1] / monthly_life_table$Lxm[j]
}
monthly_life_table$qx = 1 - monthly_life_table$px
save( monthly_life_table ,file = "MonthlyLifeTable.RData")
