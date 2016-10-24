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

#only considering the data where a person said yes to that partner, since this will help us see if the qualities they rate their partner highly in are the same ones they prioritize as being important to them
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

#trying a different approach where we compare the weighted average of the sum of scores given by participants to partners with the decision variable
#selecting only the columns from the entire data that we are interested in
attach(full_data)
dataimp<-subset(full_data, select=c(dec, attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1,attr,sinc,intel,fun,amb,shar, imprace, samerace, int_corr))
attr1=attr1_1*attr
sinc1=sinc1_1*sinc
intel1=intel1_1*intel
fun1=fun1_1*fun
amb1=amb1_1*amb
shar1=shar1_1*shar
race1=imprace*samerace
actshare1=shar1_1*int_corr
actual_corr<-lm(dec~attr1+sinc1+intel1+fun1+amb1+shar1+race1+actshare1, family=binomial)
summary(actual_corr)
#Call:
#lm(formula = dec ~ attr1 + sinc1 + intel1 + fun1 + amb1 + shar1 + 
#    race1 + actshare1, family = binomial)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.11658 -0.37464 -0.08696  0.40561  1.24289 

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -5.511e-01  2.571e-02 -21.436  < 2e-16 ***
#attr1        1.652e-03  5.995e-05  27.555  < 2e-16 ***
#sinc1        1.133e-03  9.232e-05  12.275  < 2e-16 ***
#intel1       1.106e-03  9.349e-05  11.834  < 2e-16 ***
#fun1         2.032e-03  1.017e-04  19.983  < 2e-16 ***
#amb1         3.120e-04  1.150e-04   2.712  0.00671 ** 
#shar1        3.031e-03  1.275e-04  23.764  < 2e-16 ***
#race1       -1.946e-04  1.901e-03  -0.102  0.91845    
#actshare1    9.178e-04  1.300e-03   0.706  0.48024    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4355 on 6923 degrees of freedom
#  (1446 observations deleted due to missingness)
#Multiple R-squared:  0.2287,	Adjusted R-squared:  0.2278 
#F-statistic: 256.6 on 8 and 6923 DF,  p-value: < 2.2e-16

dec_predict<-predict(actual_corr,full_data, type=response)

glm.pred=rep(0,8378)
glm.pred[dec_predict>.5]=1
table(glm.pred,dec)
mean(glm.pred==dec)
#> table(glm.pred,dec)
#        dec
#glm.pred    0    1
       0 4033 1656
       1  827 1862
#mean(glm.pred==dec)
#[1] 0.7036286
