source("Policy_Values.R")

P1 = c()
    P0 = c()
    j = 1
    for(q in 2:((input$n1*12)-2)){
      P1[j] = Policy_Value_1(AGEP =  input$Age, n1 = input$n1 , n2 = input$n2 , m = as.numeric(input$m)  , rate = input$rate , b = input$S1 , t = q ,
                             MONTH = MONTH ,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                             MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                             COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                             SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                             SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA)
      P0[j] = Policy_Value_0(AGEP =  input$Age, n1 = input$n1 , n2 = input$n2 , m = as.numeric(input$m)  , rate = input$rate , b = input$S1 , t = q ,
                             MONTH = MONTH ,  PINCP =PINCP ,  SEX_1 =SEX_1 ,  SEX_2  =SEX_2 , MAR_1 =MAR_1 ,  MAR_2  =MAR_2 , MAR_3 =MAR_3 ,
                             MAR_4 =MAR_4 ,  MAR_5  =MAR_5 ,COW_1 =COW_1 ,  COW_2 =COW_2 , COW_3 =COW_3 ,  COW_4  =COW_4 , COW_5 =COW_5 ,  COW_6  =COW_6 , COW_7 =COW_7 ,
                             COW_8 =COW_8 ,  COW_9 =COW_9 ,  COW_NA =COW_NA , MIL_1  =MIL_1 , MIL_2  =MIL_2 , MIL_3 =MIL_3  ,  MIL_4  =MIL_4 ,MIL_NA =MIL_NA , SCHG_1 =SCHG_1 ,
                             SCHG_2 =SCHG_2 , SCHG_3  =SCHG_3 , SCHG_4 =SCHG_4 , SCHG_5 =SCHG_5 ,SCHG_6 =SCHG_6 , SCHG_7 =SCHG_7 , SCHG_8 =SCHG_8 , SCHG_9 =SCHG_9 , SCHG_10 =SCHG_10 ,
                             SCHG_11= SCHG_11 , SCHG_12 =SCHG_12 , SCHG_13 =SCHG_13 , SCHG_14 =SCHG_14 , SCHG_15 =SCHG_15 ,SCHG_16 =SCHG_16 , SCHG_NA=SCHG_NA)
      j = j+1
    }
    time = 1:length(P0)
    data = data.frame(values = c(P0,P1) , group = rep(factor(c("وضعیت سالم","وضعیت ازکارافتاده")), each = j-1) , time)
    
    p = ggplot(data, aes(y = values, x = time , color = group  )) +
      geom_line() + geom_point() + transition_reveal(time) + ylab("Value") + xlab("Month")
  
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)}
