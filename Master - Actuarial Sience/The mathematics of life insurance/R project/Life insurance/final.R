library(tidyverse)
comprss <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 3), 
        c("","K","M","B","T")[div] )}
#importing annual Life table
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

#Making annual interest rate table
annual_interest_rate = data.frame(
  year = 1:30,
  interest = c(rep(0.15 , 10) , rep(0.1,20))
)
for(i in 1:nrow(annual_interest_rate)){
  annual_interest_rate$nu[i] = 1 / (1+annual_interest_rate$interest[i])
}

#Making Monthly interest rate table
monthly_interest_table = data.frame(
  Month = cumsum(rep(1/12 , nrow(annual_interest_rate) * 12)) ,
  interest_monthly = c(rep(0.15/12 , 10*12) , rep(0.1/12 , 20*12))
)
for(i in 1:nrow(monthly_interest_table)){
  monthly_interest_table$nu[i] = (1 - (1+monthly_interest_table$interest_monthly[i])^-1) / monthly_interest_table$interest_monthly[i]
}
monthly_interest_table$nu = round(monthly_interest_table$nu,5)


#Part1 Term insurance
elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y,telorance = 1.5e-5))})

term_insurance =function(x , n , m ){
  if(m == 12){
    sum_term_value = 0;j=1
    mm = monthly_interest_table$nu[(1:(m*n))] ^ (((0:((m*n) -1))+2)/12)
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = index1 + 1
    index3 = 12*x
  for(k in 0:((m*n)-1)){
sum_term_value = sum_term_value + mm[j] *(((monthly_life_table$Lxm[index1[j]]) - (monthly_life_table$Lxm[index2[j]]) ) / (monthly_life_table$Lxm[index3]))
     j = j+1
}
  }
  if(m == 1){
    mm = monthly_interest_table$nu ^ (((0:((m*n) -1))+2)/1)
    sum_term_value = 0;j=1
    index1 = (x+1) : (x+n)
    index2 = index1 + 1 
    index3 = x+1
    for(k in 0:((m*n)-1)){
      sum_term_value = sum_term_value + mm[j]*(((monthly_life_table$Lxm[index1[j]]) - (monthly_life_table$Lxm[index2[j]]) ) / (monthly_life_table$Lxm[index3]))
    }
  }
  return(sum_term_value)
  }


#Part2_1 pure endowment

pure_endowment = function(x , n){
  nEx = (annual_interest_rate$nu[n] ^ n ) * (annual_life_table$Lx[x+n+1] /annual_life_table$Lx[x+1] )
return(nEx)
  }


#Part2_2 Term life due annuties mthly per year
term_annuty = function(x , n , m){
  if(m == 12){
    mm = monthly_interest_table$nu[1:((m*n) -1)]^ ((1:((m*n) -1))/12)
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-1)){
      sum_term_value = sum_term_value + (mm[j])*(1/m)*((monthly_life_table$Lxm[index1[j]]) / (monthly_life_table$Lxm[index2]))
    j = j+1
      }
  }
  if(m == 1){
    mm = annual_interest_rate$nu[1:((m*n) -1)]^ (1:((m*n) -1))
    sum_term_value = 1;j=1
    index1 = (x+2) : (x+n)
    index2 = x+1
    for(k in 1:((m*n)-1)){
      sum_term_value = sum_term_value + (mm[j])*(1/m)*((annual_life_table$Lx[index1[j]]) / (annual_life_table$Lx[index2]))
    j = j+1
    }
  }
  return(sum_term_value)
  
}


# CalCulate All Benefits
Final_Benefits = function(Age , N1 , N2 , S1 , S2 , m){
  term_insurance_value = S1 * term_insurance(x = Age , n = N1 , m )
  Annuties_value = S2 * (term_annuty(Age , N2 , m) * pure_endowment(Age , N2))
  Total_Benefits= term_insurance_value + Annuties_value
  return(Total_Benefits)
}

#Calculate premiums
Final_Premium = function(Age , N1 , m){
  Total_net_premium= term_annuty(Age , N1 , m)
  return(Total_net_premium)
}


#Calculate Expenses
Final_Expense = function(Total_net_premium , S1){
  Total_Expense = ifelse((0.003 * S1) < (0.75 * Total_net_premium) ,(0.75 * Total_net_premium) , (0.003 * S1))
return(Total_Expense)
}

#father death coverage :



father_death = function(father_Age , Age , N1 , N2 , S1 , S2 , m){
  j = 1;amounts = c()
  while((12*N1) - j >= 0 ){
    amounts[j] = ((12*N1) - j ) * (((Final_Benefits(Age , N1 , N2 , S1 , S2 , m))/(12*Final_Premium(Age , N1 , m))) + (((0.3 * Final_Expense(((Final_Benefits(Age , N1 , N2 , S1 , S2 , m)) /(12*Final_Premium(Age , N1 , m)))  , S1)) + (0.175 * term_annuty(Age , 3 , 1))) / (12 * term_annuty(Age , 5 , 12))))

    j = j+1
  }
    mm = monthly_interest_table$nu[(1:(m*N1))] ^ (((0:((m*N1) -1))+2)/12)
    index1 = (father_Age*12) :((father_Age*12)+(N1*m) - 1)
    index2 = index1 + 1
    index3 = 12*father_Age
      sum = sum((amounts * (mm*(((monthly_life_table$Lxm[index1]) - (monthly_life_table$Lxm[index2]) ) / (monthly_life_table$Lxm[index3]))))[1:((m*(N1-1)))])
  outputs = list(sum = sum , father.P = 0.05*sum)
  return(outputs)
}

#####
####

final_function=function(father_Age , Age, N1 , N2 , S1 , S2 , m , DAD_Coverage = "yes"){
ifelse(((m!=1) &( m!=12) | (N1 < 5)),stop(call.="please enter m in {1,2} or N >5") , print("please wait a moment to see your outputs :"))
Total_Benefits = Final_Benefits(Age , N1 , N2 , S1 , S2 , m)
Total_net_premium = Total_Benefits /(m*Final_Premium(Age , N1 , m))
Total_Expense = Final_Expense(Total_net_premium  , S1)
Total_Expense_PV_0 = (0.3 * Total_Expense) + (0.175 * term_annuty(Age , 3 , 1))
mthly_Expense = Total_Expense_PV_0 / (m * term_annuty(Age , 5 , m))


if(DAD_Coverage == "yes"){
  father.death.premium = father_death(father_Age,Age , N1 , N2 , S1 , S2 , m)$sum
  father.death.expense = father_death(father_Age,Age , N1 , N2 , S1 , S2 , m)$father.P
  father_death_coverage_premium = father_death(father_Age,Age , N1 , N2 , S1 , S2 , m )$sum
  father_death_coverage_expense = father_death(father_Age,Age , N1 , N2 , S1 , S2 , m )$father.P

}else{
  father.death.premium = 0
  father.death.expense = 0 
  father_death_coverage_premium = 0
  father_death_coverage_expense= 0
}

d = cbind(Total_Benefits , Total_net_premium , Total_Expense ,
          Total_Expense_PV_0 ,mthly_Expense,
          father_death_coverage_premium,father_death_coverage_expense)

if(m == 12){
table = data.frame(year = rep(0:29 , each = 12) ,
                   month = rep(1:12 , 30),
                   net.P = Total_net_premium,
                   Expense = c(rep(mthly_Expense , 5*m) , rep(0 , 25*m)))
table$gross.P.without.father.death.coverage = table$net.P + table$Expense 


table$father.Death.coverage.P = c(father.death.premium,rep(0,(12*30)-1))
table$father.Death.coverage.Expense = c(father.death.expense,rep(0,(12*30)-1))

table$gross.P.with.father.death.coverage = table$net.P + table$Expense  + table$father.Death.coverage.P + table$father.Death.coverage.Expense
table = table[1:(m*N1),]
}else{
  table = data.frame(year = 1:30,
                     net.P = Total_net_premium,
                     Expense = c(rep(mthly_Expense , 5*m) , rep(0 , 25*m)))
  table$gross.P.without.father.death.coverage = table$net.P + table$Expense 
  table$father.Death.coverage.P = c(father.death.premium,rep(0,29))
  table$father.Death.coverage.Expense = c(father.death.expense,rep(0,29))
  table$gross.P.with.father.death.coverage = table$net.P + table$Expense  + table$father.Death.coverage.P + table$father.Death.coverage.Expense
  table = table[1:(m*N1),]
}
outputs = list(
  Calculations = d,
  Table = table
)
return(outputs)
}
#  input = list(
#   DAD_Age = 10 , Age = 10 , N1 = 10 , N2 = 10 , S1 =1 , S2 = 1 , m = 1 , DAD_Coverage = "no"
#  )
# a2 = final_function(father_Age = input$DAD_Age ,Age = input$Age, N1 = input$N1 , N2 = input$N2 , S1 = input$S1, S2= input$S2 , m = input$S2, DAD_Coverage = input$DAD_Coverage)
# View(a2$Table)
# View(a2$Calculations)
