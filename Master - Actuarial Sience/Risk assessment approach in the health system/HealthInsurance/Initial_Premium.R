source("MakingMatrix.R")
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



# Term life due annuties mthly per year
term_annuty = function(x , n , m , rate = 0.05  ,MONTH = 1 ,  PINCP =100000 ,  SEX_1 =0 ,  SEX_2  =1 , MAR_1 =0 ,  MAR_2  =0 , MAR_3 =0 ,
                       MAR_4 =0 ,  MAR_5  =1 ,COW_1 =0 ,  COW_2 =0 , COW_3 =0 ,  COW_4  =0 , COW_5 =0 ,  COW_6  =0 , COW_7 =0 ,
                       COW_8 =0 ,  COW_9 =0 ,  COW_NA =1 , MIL_1  =0, MIL_2  =0 , MIL_3 =0 ,  MIL_4  =0 ,MIL_NA =1 , SCHG_1 =0 ,
                       SCHG_2 =0 , SCHG_3  =0 , SCHG_4 =0 , SCHG_5 =0 ,SCHG_6 =0 , SCHG_7 =0 , SCHG_8 =0 , SCHG_9 =0 , SCHG_10 =0 ,
                       SCHG_11=0 , SCHG_12 =0 , SCHG_13 =0 , SCHG_14 =0 , SCHG_15 =0 ,SCHG_16 =0 , SCHG_NA=1){
  if(m == 12){
    mm = monthly_interest_table$nu[1:((m*n) -1)]^ ((1:((m*n) -1))/12)
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-1)){
      sum_term_value = sum_term_value + (((1 + rate)^k) * (mm[j])*(1/m)*MatrixFunction(x = x ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                       MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                       COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                       SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                       SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA, t = k)[1,1])
      j = j+1
    }
  }

  return(sum_term_value)
  
}

Initial_Premimum = function(x , n1 , n2 , m = 12 , rate = 0.05  , b = 10000 , MONTH = 1 ,  PINCP =100000 ,  SEX_1 =0 ,  SEX_2  =1 , MAR_1 =0 ,  MAR_2  =0 , MAR_3 =0 ,
                            MAR_4 =0 ,  MAR_5  =1 ,COW_1 =0 ,  COW_2 =0 , COW_3 =0 ,  COW_4  =0 , COW_5 =0 ,  COW_6  =0 , COW_7 =0 ,
                            COW_8 =0 ,  COW_9 =0 ,  COW_NA =1 , MIL_1  =0, MIL_2  =0 , MIL_3 =0 ,  MIL_4  =0 ,MIL_NA =1 , SCHG_1 =0 ,
                            SCHG_2 =0 , SCHG_3  =0 , SCHG_4 =0 , SCHG_5 =0 ,SCHG_6 =0 , SCHG_7 =0 , SCHG_8 =0 , SCHG_9 =0 , SCHG_10 =0 ,
                            SCHG_11=0 , SCHG_12 =0 , SCHG_13 =0 , SCHG_14 =0 , SCHG_15 =0 ,SCHG_16 =0 , SCHG_NA=1){
  output = ((term_annuty(x  = x , n = n1 , m  = m, rate = rate  ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                         MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                         COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                         SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                         SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA) * (b/12)) / (term_annuty(x = x , n = n2 , m  = m, rate = rate ,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                                                                                                                    MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                                                                                                                    COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                                                                                                                    SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                                                                                                                    SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA))) 
return(output)
}


#Initial_Premimum(x = 55, n1 = 10 , n2 = 5 , m = 12  , rate = 0.1 , b = 10000 )

