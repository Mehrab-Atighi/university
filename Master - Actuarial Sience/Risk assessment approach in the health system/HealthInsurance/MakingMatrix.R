load("MonthlyLifeTable.RData")
load("MultinomialLogisticRegressionModel.RData")

##### Matrix powers ###############################
# matrixpower(mat,k) mat^k
#
matrixpower <- function(mat,k) {
  if (k == 0) return (diag(dim(mat)[1])) 
  if (k == 1) return(mat)
  if (k > 1) return( mat %*% matrixpower(mat, k-1))
}


MatrixFunction = function(x  =10 ,MONTH = 1 , t = 100,  PINCP =100000 ,  SEX_1 =0 ,  SEX_2  =1 , MAR_1 =0 ,  MAR_2  =0 , MAR_3 =0 ,
                          MAR_4 =0 ,  MAR_5  =1 ,COW_1 =0 ,  COW_2 =0 , COW_3 =0 ,  COW_4  =0 , COW_5 =0 ,  COW_6  =0 , COW_7 =0 ,
                          COW_8 =0 ,  COW_9 =0 ,  COW_NA =1 , MIL_1  =0, MIL_2  =0 , MIL_3 =0 ,  MIL_4  =0 ,MIL_NA =1 , SCHG_1 =0 ,
                          SCHG_2 =0 , SCHG_3  =0 , SCHG_4 =0 , SCHG_5 =0 ,SCHG_6 =0 , SCHG_7 =0 , SCHG_8 =0 , SCHG_9 =0 , SCHG_10 =0 ,
                          SCHG_11=0 , SCHG_12 =0 , SCHG_13 =0 , SCHG_14 =0 , SCHG_15 =0 ,SCHG_16 =0 , SCHG_NA=1){

  new_data = data.frame(AGEP  =x ,MONTH = MONTH,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                      MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                      COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                      SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                      SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA)
ml = predict.glm(ml1 , newdata = new_data , type = "response")

#Make Transport Probability matrix
year_index = which(monthly_life_table[,c("year")] == x)
month_index = which(monthly_life_table[year_index,c("Month")] == MONTH)
final_index = year_index[month_index]

initial_P = matrix(c ( (0.99 * (1-ml/12)) , (0.99/12)*(ml) , 0.01 ,
              0 , (1-monthly_life_table[c(final_index),7]) , (monthly_life_table[c(final_index),7]) ,
              0 , 0 , 1) , 
           ncol = 3 , byrow = TRUE)

####################
t = t
final_P = matrixpower(initial_P , t)

return(final_P)
}
# MatrixFunction(x = 86 ,  , t = 200)

