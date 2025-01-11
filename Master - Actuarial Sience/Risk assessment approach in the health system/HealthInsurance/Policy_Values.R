source("Initial_Premium.R")
library(gganimate)
library(ggplot2)
# Term life due annuties mthly per year
part0_1 = function(x , n , m = 12 , rate = rate , t , MONTH = MONTH,
                   PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                   MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                   COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                   SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                   SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA){
  if(m == 12){
    mm = monthly_interest_table$nu[1:((m*n) -1)]^ ((1:((m*n) -1))/12)
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-t)){
      sum_term_value = sum_term_value + (((1 + rate)^(t+k)) * (mm[j])*(1/m)*MatrixFunction(x =x ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                           MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                           COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                           SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                           SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA , t = k)[1,2])
      j = j+1
    }
  }
  
  return(sum_term_value)
  
}

part0_2 = function(x , n1 , n2 , m = 12 , rate = rate , t , b = b ,MONTH = MONTH,
                   PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                   MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                   COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                   SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                   SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA
                    ){
  n = n1
  rrr = Initial_Premimum(x  = x, n1 = n1 , n2 = n2, m = 12 , rate = rate  , b = b , MONTH = MONTH, PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                         MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                         COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                         SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                         SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA )
  t = max(c((n*m) - t - 1 ,0 ))
  if(m == 12){
    mm = monthly_interest_table$nu[1:((m*n) -1)]^ t
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-t)){
      sum_term_value = sum_term_value + (rrr * ((1 + rate)^(t+k)) * (mm[j])*(1/m)*MatrixFunction(x  =x , MONTH = MONTH, PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                                 MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                                 COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                                 SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                                 SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA , t = k)[1,1])
      j = j+1
    }
  }
  
  return(sum_term_value)
  
}


#calculating policy value for h = 0 for all month all over the treaty
# t = 1

Policy_Value_0 = function(n1 , n2 , m = 12 , rate = 0.1  , b = 10000 , t ,AGEP = 55, MONTH = 1 ,  PINCP =100000 ,  SEX_1 =0 ,  SEX_2  =1 , MAR_1 =0 ,  MAR_2  =0 , MAR_3 =0 ,
                          MAR_4 =0 ,  MAR_5  =1 ,COW_1 =0 ,  COW_2 =0 , COW_3 =0 ,  COW_4  =0 , COW_5 =0 ,  COW_6  =0 , COW_7 =0 ,
                          COW_8 =0 ,  COW_9 =0 ,  COW_NA =1 , MIL_1  =0, MIL_2  =0 , MIL_3 =0 ,  MIL_4  =0 ,MIL_NA =1 , SCHG_1 =0 ,
                          SCHG_2 =0 , SCHG_3  =0 , SCHG_4 =0 , SCHG_5 =0 ,SCHG_6 =0 , SCHG_7 =0 , SCHG_8 =0 , SCHG_9 =0 , SCHG_10 =0 ,
                          SCHG_11=0 , SCHG_12 =0 , SCHG_13 =0 , SCHG_14 =0 , SCHG_15 =0 ,SCHG_16 =0 , SCHG_NA=1
                          ){
  output = (part0_1(x = AGEP , n = n1 , m = m , rate = rate , t = t ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                    MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                    COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                    SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                    SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA) * (b/12)) - ((Initial_Premimum(AGEP , n1 , n2 , m , rate , b))*part0_2(x = AGEP , n1 , n2 , m = 12 , rate = rate , t = t , b = b ,MONTH = MONTH ,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                                                                                                                                                             MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                                                                                                                                                             COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                                                                                                                                                             SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                                                                                                                                                             SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA))
  return(output)
}


# Term life due annuties mthly per year
part1_1 = function(x = AGEP , n , m = 12 , rate = rate , t , MONTH = MONTH,
                   PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                   MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                   COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                   SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                   SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA){
  if(m == 12){
    mm = monthly_interest_table$nu[1:((m*n) -1)]^ ((1:((m*n) -1))/12)
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-t)){
      sum_term_value = sum_term_value + (((1 + rate)^(t+k)) * (mm[j])*(1/m)*MatrixFunction(x  =x , MONTH = MONTH, PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                           MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                           COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                           SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                           SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA , t = k)[2,2])
      j = j+1
    }
  }
  
  return(sum_term_value)
  
}

part1_2 = function(x = AGEP , n1 , n2 , m = 12 , rate = rate , t , b = b  , MONTH = MONTH,
                   PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                   MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                   COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                   SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                   SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA){
  n = n1
  rrr = Initial_Premimum(x  = x, n1 = n1 , n2 = n2, m = 12 , rate = rate  , b = b , MONTH = MONTH, PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                         MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                         COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                         SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                         SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA)
  t = max(c((n*m) - t - 1 ,0 ))
  if(m == 12){
    mm = monthly_interest_table$nu[1:((m*n) -1)]^ t
    sum_term_value = 0;j=1
    index1 = (x*12) :((x*12)+(n*m) - 1)
    index2 = 12*x
    for(k in 1:((m*n)-t)){
      sum_term_value = sum_term_value + (rrr * ((1 + rate)^(t+k)) * (mm[j])*(1/m)*MatrixFunction(x  =x ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                                 MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                                 COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                                 SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                                 SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA, t = k)[2,1])
      j = j+1
    }
  }

  return(sum_term_value)

}


#calculating policy value for h = 0 for all month all over the treaty
# t = 1

Policy_Value_1 = function(AGEP = 55 , n1 , n2 , m = 12 , rate = 0.1  , b = 10000 , t , MONTH = 1 ,  PINCP =100000 ,  SEX_1 =0 ,  SEX_2  =1 , MAR_1 =0 ,  MAR_2  =0 , MAR_3 =0 ,
                          MAR_4 =0 ,  MAR_5  =1 ,COW_1 =0 ,  COW_2 =0 , COW_3 =0 ,  COW_4  =0 , COW_5 =0 ,  COW_6  =0 , COW_7 =0 ,
                          COW_8 =0 ,  COW_9 =0 ,  COW_NA =1 , MIL_1  =0, MIL_2  =0 , MIL_3 =0 ,  MIL_4  =0 ,MIL_NA =1 , SCHG_1 =0 ,
                          SCHG_2 =0 , SCHG_3  =0 , SCHG_4 =0 , SCHG_5 =0 ,SCHG_6 =0 , SCHG_7 =0 , SCHG_8 =0 , SCHG_9 =0 , SCHG_10 =0 ,
                          SCHG_11=0 , SCHG_12 =0 , SCHG_13 =0 , SCHG_14 =0 , SCHG_15 =0 ,SCHG_16 =0 , SCHG_NA=1
                          ){
  output = (part1_1(x = AGEP , n = n1 , m = m , rate = rate , t = t , MONTH = MONTH,
                      PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                    MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                    COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                    SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                    SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA) * (b/12)) - part1_2(x = AGEP , n1 , n2 , m = 12 , rate = rate , t = t , b = b ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                                                                                                                                                                          MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                                                                                                                                                                          COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                                                                                                                                                                          SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                                                                                                                                                                          SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA)
  return(output)
}


#Policy_Value_1(AGEP = 20, n1 = 3 , n2 =2 , m = 12 , rate = 0.1 , b = 10 , t = 5)
#Policy_Value_0(AGEP = 20,n1 = 3 , n2 =2 , m = 12 , rate = 0.1 , b = 10 , t = 5)


