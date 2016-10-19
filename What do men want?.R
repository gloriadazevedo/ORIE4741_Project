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
