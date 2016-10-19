#Trying to see which attributes make men more likely to say yes to a woman

library(gdata)
setwd("/Users/pihu_yadav/Downloads/ORIE4741_Project-master")
speeddating=read.csv("data_excel.csv")

#selecting all the participants that are male
mendata<- subset(speeddating, gender==1)

#seeing all the women they said yes to
menlike<- subset(mendata, dec==1)

#seeing the summaries of some key statistics that might be deciding the decision
#these include: whether both are of the same race, their interests correlate, the woman's levels of attractiveness, sincerity, intelligence, fun, ambition and shared interests as determined by the man at the end of the speed date
menattr<- subset(menlike, select = c(samerace, int_corr, attr, sinc, intel, fun, amb, shar))
summary(menattr)

#seeing the total values of the above features (i.e. including the results of data where the man said no to the woman) in order to provide a comparison between all women and the women they said yes to
mentotalattr <- subset(mendata , select = c(samerace, int_corr, attr, sinc, intel, fun, amb, shar))
summary(mentotalattr)

#Output for what men want (women they said yes to)
#summary(menattr)
#    samerace         int_corr           attr             sinc       
# Min.   :0.0000   Min.   :-0.830   Min.   : 2.000   Min.   : 0.000  
# 1st Qu.:0.0000   1st Qu.:-0.010   1st Qu.: 6.000   1st Qu.: 7.000  
# Median :0.0000   Median : 0.200   Median : 7.000   Median : 8.000  
# Mean   :0.3957   Mean   : 0.195   Mean   : 7.449   Mean   : 7.568  
# 3rd Qu.:1.0000   3rd Qu.: 0.430   3rd Qu.: 8.000   3rd Qu.: 8.000  
# Max.   :1.0000   Max.   : 0.900   Max.   :10.000   Max.   :10.000  
#                                   NA's   :7        NA's   :29      
#     intel             fun              amb              shar       
# Min.   : 3.000   Min.   : 0.000   Min.   : 2.000   Min.   : 0.000  
# 1st Qu.: 7.000   1st Qu.: 6.000   1st Qu.: 6.000   1st Qu.: 5.000  
# Median : 8.000   Median : 7.000   Median : 7.000   Median : 6.000  
# Mean   : 7.621   Mean   : 7.296   Mean   : 6.995   Mean   : 6.372  
# 3rd Qu.: 8.000   3rd Qu.: 8.000   3rd Qu.: 8.000   3rd Qu.: 8.000  
# Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000  
# NA's   :26       NA's   :49       NA's   :134      NA's   :181   


#Output for their general responses (for comparison)
#summary(mentotalattr)
#    samerace         int_corr           attr            sinc       
# Min.   :0.0000   Min.   :-0.830   Min.   : 0.00   Min.   : 0.000  
# 1st Qu.:0.0000   1st Qu.:-0.010   1st Qu.: 5.00   1st Qu.: 6.000  
# Median :0.0000   Median : 0.200   Median : 7.00   Median : 7.000  
# Mean   :0.3953   Mean   : 0.192   Mean   : 6.46   Mean   : 7.251  
# 3rd Qu.:1.0000   3rd Qu.: 0.430   3rd Qu.: 8.00   3rd Qu.: 8.000  
# Max.   :1.0000   Max.   : 0.910   Max.   :10.00   Max.   :10.000  
#                                   NA's   :101     NA's   :147     
#     intel            fun             amb              shar      
# Min.   : 0.00   Min.   : 0.00   Min.   : 0.000   Min.   : 0.00  
# 1st Qu.: 6.00   1st Qu.: 5.00   1st Qu.: 5.000   1st Qu.: 4.00  
# Median : 7.00   Median : 7.00   Median : 7.000   Median : 6.00  
# Mean   : 7.29   Mean   : 6.52   Mean   : 6.603   Mean   : 5.54  
# 3rd Qu.: 8.00   3rd Qu.: 8.00   3rd Qu.: 8.000   3rd Qu.: 7.00  
# Max.   :10.00   Max.   :10.00   Max.   :10.000   Max.   :10.00  
# NA's   :149     NA's   :167     NA's   :350      NA's   :491   
