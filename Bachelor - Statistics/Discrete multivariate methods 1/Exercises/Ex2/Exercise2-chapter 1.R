Data<-array(data = c(7,11,13,9,4,5,23,17,1,2,3,13,0,2,1,8,
                     7,14,6,4,4,5,9,11,2,1,18,14,1,0,2,22),
            c(4,4,2))
Data
#f=ftable(Data)
#f

#dakhele Moaser:

#baraye moghayese y1 : (<20 & 20-30) :
(d1=cbind(Data [1 , , 1] , Data [2 , , 1]))
chisq.test(d1)
prop.test(d1)
fisher.test(d1)

#baraye moghayese y1: (<20 & 30-60) :
(d2=cbind(Data [1 , , 1] , Data [3 , , 1]))
chisq.test(d2)
prop.test(d2)
fisher.test(d2)

#baraye moghayese y1: (<20 & >60) :
(d3=cbind(Data [1 , , 1] , Data [4 , , 1]))
chisq.test(d3)
prop.test(d3)
fisher.test(d3)

#baraye moghayese y1: (20-30 & 30-60) :
(d4=cbind(Data [2, , 1] , Data [3 , , 1]))
chisq.test(d4)
prop.test(d4)
fisher.test(d4)

#baraye moghayese y1: (20-30 & >60) :
(d5=cbind(Data [2, , 1] , Data [4 , , 1]))
chisq.test(d5)
prop.test(d5)
fisher.test(d5)

#baraye moghayese y1: (30-60 & 60>) :
(d6=cbind(Data [3, , 1] , Data [4 , , 1]))
chisq.test(d6)
prop.test(d6)
fisher.test(d6)

####################
#dakhele daronema:

#baraye moghayese y1 : (<20 & 20-30) :
(d7=cbind(Data [1 , , 2] , Data [2 , , 2]))
chisq.test(d7)
prop.test(d7)
fisher.test(d7)

#baraye moghayese y1: (<20 & 30-60) :
(d8=cbind(Data [1 , , 2] , Data [3 , , 2]))
chisq.test(d8)
prop.test(d8)
fisher.test(d8)

#baraye moghayese y1: (<20 & >60) :
(d9=cbind(Data [1 , , 2] , Data [4 , , 2]))
chisq.test(d9)
prop.test(d9)
fisher.test(d9)

#baraye moghayese y1: (20-30 & 30-60) :
(d10=cbind(Data [2, , 2] , Data [3 , , 2]))
chisq.test(d10)
prop.test(d10)
fisher.test(d10)

#baraye moghayese y1: (20-30 & >60) :
(d11=cbind(Data [2, , 2] , Data [4 , , 2]))
chisq.test(d11)
prop.test(d11)
fisher.test(d11)

#baraye moghayese y1: (30-60 & 60>) :
(d12=cbind(Data [3, , 2] , Data [4 , , 2]))
chisq.test(d12)
prop.test(d12)
fisher.test(d12)


################
#koli:
s1=c()
s2=c()
for(i in 1:4){
s1[i]=sum(Data[i,,1])
s2[i]=sum(Data[i,,2])
}
a1=cbind(s1,s2)
rownames(a1)=c("<20" ,"20-30" ,"30-60" ,">60")
colnames(a1)=c("Moaser" ,"Daronema")
a1
chisq.test(a1)
prop.test(a1)
fisher.test(a1)



