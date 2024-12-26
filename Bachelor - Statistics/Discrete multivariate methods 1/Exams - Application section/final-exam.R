


#multinomial 

W = 2 *  (20*log(20/25) + (30 * log(30/25)) + 20*log(20/25) + (30 * log(30/25)) )
W
(khi2 = qchisq( 0.95 , df = 3))

ifelse(khi2 > W , "H0 Accept" ,"H0 reject")

#Possion

W = 2 * (((17*log(17/14.8)) + (14.8 - 17) ) + ((7*log(7/14.8))+(14.8 - 7)) +
           ((6*log(6/14.8))+(14.8 - 6)) + ((22*log(22/19.6))+(29.6 - 22)) ) 
W
(khi2 = qchisq( 0.99 , df = 3))

ifelse(khi2 > W , "H0 Accept" ,"H0 reject")
