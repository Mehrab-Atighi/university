
salary_increase = 0.17
interest_per_year = 0.2
interest_per_month = interest_per_year / 12
r = 65


#start
Table = readxl::read_excel(path ="Book1.xlsx" , sheet = "Table" ,range = "G2:M1236")
Table2 = readxl::read_excel(path ="Book1.xlsx" , sheet = "Table" ,range = "A2:E109")
Popluation = readxl::read_excel(path ="Book1.xlsx" , sheet = "Information" )

#some transform in popluation and Table data:
Popluation$Experience = Popluation$`Today Age` -  Popluation$`Hire Age` 
Table$nu = (1 - (1+interest_per_month)^-1) / interest_per_month
Annual_nu = 1 / (1+interest_per_year)
Annual_nu_j = 1 / (1 + (1 + interest_per_year) / (1 + salary_increase) -1)
####Individual Part####
#EAN for each individual

for(i in 1:nrow(Popluation)){
  Salaries = Popluation$`Salary At Hire`[i] * (1+salary_increase)^c(1:Popluation$Experience[i])
  
  if(Popluation$Experience[i] >=10 & Popluation$Experience[i] < 20){
    dif = 20 - Popluation$Experience[i]
    B = sum(Salaries[1:10]) + 2*sum(Salaries[11:(20-dif)]) /(10+ (20-dif)*2 )
  }
  if(Popluation$Experience[i] >=20 & Popluation$Experience[i] < 30){
    dif = 30 - Popluation$Experience[i]
    B = sum(Salaries[1:10]) + 2*sum(Salaries[11:20]) + 3*sum(Salaries[21:(30-dif)]) /(10+ 20 + (30-dif)*3 )
  }
  if(Popluation$Experience[i] >=30 & Popluation$Experience[i] < (min(35 , 65-Popluation$`Hire Age`[i]))){
    dif = 5 - (35 - Popluation$Experience[i])
    ifelse(dif == 0 , 1 , dif)
    B = (sum(Salaries[dif :(10 + dif)]) + 2*sum(Salaries[(11 + dif):(20 + dif)]) + 3*sum(Salaries[(21+dif):(30+dif)])) / 60
  }
  Br = c()
  if(Popluation$Experience[i] <10){B = 0}
}

for(i in 1:nrow(Popluation)){
  All_Salaries = Popluation$`Salary At Hire`[i] * (1+salary_increase)^c(1:35)
  Br[i] =  (sum(All_Salaries[1:10]) + 2*sum(All_Salaries[11 :20]) + 3*sum(All_Salaries[21:30])) / 60
}


#we need to define anniuty term function here:
# Term life due annuties mthly per year
term_annuty = function(x , n , m){
  if(m == 12){
    if(n <= 0){n = 1}
    mm = Table$nu[1:((m*n) -1)]^ ((1:((m*n) -1))/12)
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-1)){
      sum_term_value = sum_term_value + (mm[j])*(1/m)*((Table$Lx[index1[j]]) / (Table$Lx[index2]))
      j = j+1
    }
  }
  if(m == 1){
    if(n <= 0){n = 1}
    mm = rep(Annual_nu_j , ((m*n) -1))^ (1:((m*n) -1))
    sum_term_value = 1;j=1
    index1 = (x+2) : (x+n)
    index2 = x+1
    for(k in 1:((m*n)-1)){
      sum_term_value = sum_term_value + (mm[j])*(1/m)*((Table2$Lx[index1[j]]) / (Table2$Lx[index2]))
      j = j+1
    }
  }
  return(sum_term_value)
}


#Now we want to calculate the Pv(Nc) = Pv(B) at entery age


Nce=c()
for( e in 1:nrow(Popluation)){
  if(Popluation$`Wife?`[e] == "Yes"){
    Nce[e] = (Br[e] * term_annuty(Popluation$`Hire Age`[e] , n = min(35 , 65-Popluation$`Hire Age`[e]) , m = 12) *Annual_nu^(min(35 , 65-Popluation$`Hire Age`[e])) * ((Table2$`نرخ زندگی p`[Popluation$`Wife Age`[e] + 1 ]) + (Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + 1] * 0.001) + Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + 1]))/(term_annuty(Popluation$`Hire Age`[e] , n = min(35 , 65-Popluation$`Hire Age`[e]) , m = 1))
    
  }else{
    Nce[e] = (Br[e] * term_annuty(Popluation$`Hire Age`[e] , n = min(35 , 65-Popluation$`Hire Age`[e]) , m = 12) *Annual_nu^(min(35 , 65-Popluation$`Hire Age`[e])) * ((Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + 1] * 0.001) + Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + 1]))/(term_annuty(Popluation$`Hire Age`[e] , n = min(35 , 65-Popluation$`Hire Age`[e]) , m = 1))
    
  }
}


#Calculate Al prospective: at time e or e+1 or ...
## calculate pv(Nc) and pv(B) at time e or e+1 or ...
PVNC = c();PvNcT = c();FirstName= c() ; LastName = c();
PVB = c();PvBT = c();Time = c();Age = c()
for( time in  1:50 ){
  for( e in 1:nrow(Popluation)){
    if(Popluation$`Wife?`[e] == "Yes"){
      PVNC[e] = (term_annuty(Popluation$`Hire Age`[e] , n = (min(35 , 65-Popluation$`Hire Age`[e]) - time) , m = 1)) * (Nce[e] * (1+interest_per_year)^time)
      PVB[e] = (Br[e] * term_annuty(Popluation$`Hire Age`[e] , n =  (min(35 , 65-Popluation$`Hire Age`[e]) - time) , m = 12) *Annual_nu^((min(35 , 65-Popluation$`Hire Age`[e]) - time)) * ((Table2$`نرخ زندگی p`[Popluation$`Wife Age`[e] + time ]) + (Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + time] * 0.001) + Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + time]))
      
    }else{
      PVNC[e] = (term_annuty(Popluation$`Hire Age`[e] , n = (min(35 , 65-Popluation$`Hire Age`[e]) - time) , m = 1)) * Nce[e] 
      PVB[e] = (Br[e] * term_annuty(Popluation$`Hire Age`[e] , n = (min(35 , 65-Popluation$`Hire Age`[e]) - time) , m = 12) *Annual_nu^(min(35 , 65-Popluation$`Hire Age`[e]) - time) * ((Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + time] * 0.001) + Table2$`نرخ زندگی p`[Popluation$`Hire Age`[e] + time]))
    }
  }
  PvNcT = c(PvNcT , PVNC)
  PvBT = c(PvBT , PVB)
  Time = c(Time , rep(time , nrow(Popluation)))
  FirstName = c(FirstName  , Popluation$`First Name`)
  LastName = c(LastName , Popluation$`Last Name`)
  Age = c(Age , Popluation$`Hire Age` + c(time-1))
}
output = data.frame(FirstName , LastName , PvNcT , PvBT , Time , Age)
output$Al = output$PvBT - output$PvNcT
for( i in 1:nrow(output)){
  if(output$Age[i] <=20){output$Range[i] = 1}
         if(output$Age[i] >=21 & output$Age[i] <= 30){output$Range[i] =2} 
                if(output$Age[i] >=31 & output$Age[i] <= 40){output$Range[i] = 3}
                       if(output$Age[i] >=41 & output$Age[i] <= 50){output$Range[i] = 4}
                              if(output$Age[i] >=51 & output$Age[i] <= 65){output$Range[i] = 5}}

#calculate Total actuarial liability here for each time
TAL = c()
for(time in 1:50){
  TAL[time] = sum(output$Al[which(output$Time == time)])
}
output2 = data.frame(TAL , Time = 1:50)
plot(output2$Time , output2$TAL , type = "l" , col = 85)



####Aggregate Part#####

output3 = data.frame(Range = rep(1:5 , 50) , Time = rep(1:50 , each = 5)  , NUMBER_OF_INDIVIDUALS= 0)
for(r in 1:5){
  for(t in 1:50){

output3$TPVB[which(output3$Range == r & output3$Time == t)] = sum(output$PvBT[c(which(output$Range == r & output$Time==t))])
output3$TAL[which(output3$Range == r & output3$Time == t)] = sum(output$Al[c(which(output$Range == r & output$Time==t))])
output3$NUMBER_OF_INDIVIDUALS[which(output3$Range == r & output3$Time == t)]  = length(which(output$Range == r & output$Time==t ))
}
}
output3$TAL[which( output3$Time == 1)] = 0
for(r in 1:5){
  for(t in 1:49){
    output3$TAL[which(output3$Range == r & output3$Time == (t+1))] = sum(output$Al[c(which(output$Range == r & output$Time==t))]) * (1+interest_per_year)^t
  }
}

output3$TPVNC = output3$TPVB - output3$TAL
output3$Nc_Per_individual = output3$TPVNC / output3$NUMBER_OF_INDIVIDUALS


par(mfrow = c(2,2))
plot(output3$Time[which(output3$Range==1)] , output3$TAL[which(output3$Range==1)] , type = "l" , col  = 85)
plot(output3$Time[which(output3$Range==2)] , output3$TAL[which(output3$Range==2)] , type = "l" , col  = 85)
plot(output3$Time[which(output3$Range==3)] , output3$TAL[which(output3$Range==3)] , type = "l" , col  = 85)
plot(output3$Time[which(output3$Range==4)] , output3$TAL[which(output3$Range==4)] , type = "l" , col  = 85)



