#How well do you know yourself?

#This analysis aims to determine how much the values of what people said they were looking for in a partner correlate with the people they said 'yes' to
#Import data after you change into the directory where the data is stored

library(gdata)
setwd("/Users/pihu_yadav/Downloads/ORIE4741_Project-master")
full_data<-read.table("data_excel.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)



#Need to recode values of "NA" in male or female race to be 6
for (i in 1:length(full_data$race)){
	if (is.na(full_data$race[i])){
			full_data$race[i]<-6
	}
}

#First we need to recode the values that have an "NA" in their field_cd to have a field_cd of 
#18 which corresponds to the Other field
for(i in 1:length(full_data$field_cd)){
	if(is.na(full_data$field_cd[i])){
		full_data$field_cd[i]<-18
	}
}



#How well do you know yourself?

#This analysis aims to determine how much the values of what people said they were looking for in a partner, correlate with the attributes of the people they said 'yes' to

#only considering the data where a person said yes to that partner, since this will help us see if the qualities they rate their partner highly at are the same ones they prioritize as being important to them
yesdata<- subset(full_data, dec==1)
yesdataimp<-subset(yesdata, select=c(attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1,attr,sinc,intel,fun,amb,shar, imprace, samerace, int_corr))

#correlation between importance of attractiveness versus perceived attractiveness of partner

attr_corr<-lm(attr1_1~attr,data=yesdataimp)
summary(attr_corr)

#correlation between importance of sincerity versus perceived sincerity of partner

sinc_corr<-lm(sinc1_1~sinc, data=yesdataimp)
summary(sinc_corr)

#correlation between importance of intelligence versus perceived intelligence of partner

intel_corr<-lm(intel1_1~intel, data=yesdataimp)
summary(intel_corr)

#correlation between importance of being fun versus perceived 'fun-ness' of partner

fun_corr<-lm(fun1_1~fun, data=yesdataimp)
summary(fun_corr)

#correlation between importance of ambition versus perceived ambition of partner

amb_corr<-lm(amb1_1~amb, data=yesdataimp)
summary(amb_corr)

#correlation between importance of shared interests versus perceived shared interests of partner

shar_corr<-lm(shar1_1~shar,data=yesdataimp)
summary(shar_corr)

#correlation between importance of shared interests versus actual shared interests
#the actual shared interests is given by the variable int_corr, which gives the actual correlation between the interests of the two people, using the data filled out by them before the speed dating event, assigning scores to various activities and interests 

shar_corr2<-lm(shar1_1~int_corr, data=yesdataimp)
summary(shar_corr2)

#correlation between perceived shared interests and actual shared interests
#this assessment gives us an understanding of the accuracy with which a person can tell if a partner has the same interests as them after the 4 minute speed date
#this can be considered an overall reflection of the speed dating system

shar_corr3<-lm(shar~int_corr, data=yesdataimp)
summary(shar_corr3)

#correlation between importance of same race and partner actually being of same race

race_corr<-lm(imprace~samerace, data=yesdataimp)
summary(race_corr)
