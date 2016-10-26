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



#How well do you know yourself? A preliminary analysis

#This analysis aims to determine how much the values of what people said they were looking for in a partner, correlate with the attributes of the people they said 'yes' to

#Since this is only a preliminary analysis, we start by considering only the data where a person said yes to that partner, since this will help us see if the qualities they rate their partner highly in are the same ones they prioritize as being important to them

yesdata<- subset(full_data, dec==1)
yesdataimp<-subset(yesdata, select=c(attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1,attr,sinc,intel,fun,amb,shar, imprace, samerace, int_corr))

#Calculating the correlation between the importance they assign to attractiveness versus their perceived attractiveness of partner that they said 'yes' to

attr_corr<-lm(attr1_1~attr,data=yesdataimp)
summary(attr_corr)

plot(attr_corr)

#Calculating the correlation between the importance they assign to sincerity versus their perceived sincerity of partner that they said 'yes' to

sinc_corr<-lm(sinc1_1~sinc, data=yesdataimp)
summary(sinc_corr)

#Calculating the correlation between the importance they assign to intelligence versus their perceived intelligence of partner that they said 'yes' to

intel_corr<-lm(intel1_1~intel, data=yesdataimp)
summary(intel_corr)

#Calculating the correlation between the importance they assign to being fun versus their perceived 'fun-ness' of partner that they said 'yes' to

fun_corr<-lm(fun1_1~fun, data=yesdataimp)
summary(fun_corr)

#Calculating the correlation between the importance they assign to ambition versus their perceived ambition of partner that they said 'yes' to

amb_corr<-lm(amb1_1~amb, data=yesdataimp)
summary(amb_corr)

#Calculating the correlation between the importance they assign to shared interests versus their perceived shared interests with the partner that they said 'yes' to

shar_corr<-lm(shar1_1~shar,data=yesdataimp)
summary(shar_corr)

#Calculating the correlation between the importance they assign to shared interests versus actual shared interests between them and the partner
#The actual shared interests is given by the variable int_corr, which gives the actual correlation between the interests of the two people, using the data filled out by them before the speed dating event, assigning scores to various activities and interests 

shar_corr2<-lm(shar1_1~int_corr, data=yesdataimp)
summary(shar_corr2)

#Calculating the correlation between their perceived shared interests and actual shared interests
#this assessment gives us an understanding of the accuracy with which a person can tell if a partner has the same interests as them after the 4 minute speed date
#this can be considered an overall reflection of the speed dating system, and whether it gives enough time for people to really identify the characteristics of the person that they are talking to

shar_corr3<-lm(shar~int_corr, data=yesdataimp)
summary(shar_corr3)

#Calculating the correlation between importance of same race and partner they said 'yes' to actually being of the same race

race_corr<-lm(imprace~samerace, data=yesdataimp)
summary(race_corr)

#Now we try a different approach where we compare the weighted sum of scores given by participants to partners with the decision variable
#If people are able to correc

#From this, selecting only the columns from the entire data that we are interested in

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
actual_corr<-glm(dec~attr1+sinc1+intel1+fun1+amb1+shar1+race1+actshare1, family=binomial)
dec_predict<-predict(actual_corr,full_data, type=response)

summary(actual_corr)
#
#Call:
#glm(formula = dec ~ attr1 + sinc1 + intel1 + fun1 + amb1 + shar1 + 
#    race1 + actshare1, family = binomial)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.6406  -0.9010  -0.4322   0.9591   2.8742  

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -5.9196607  0.1794454 -32.989  < 2e-16 ***
#attr1        0.0093923  0.0003851  24.390  < 2e-16 ***
#sinc1        0.0065910  0.0005029  13.106  < 2e-16 ***
#intel1       0.0064759  0.0005153  12.568  < 2e-16 ***
#fun1         0.0111567  0.0005817  19.178  < 2e-16 ***
#amb1         0.0019371  0.0006147   3.151  0.00163 ** 
#shar1        0.0164823  0.0007357  22.405  < 2e-16 ***
#race1       -0.0068483  0.0100916  -0.679  0.49738    
#actshare1    0.0046008  0.0069152   0.665  0.50585    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 9487.3  on 6931  degrees of freedom
#Residual deviance: 7637.8  on 6923  degrees of freedom
#  (1446 observations deleted due to missingness)
#AIC: 7655.8

#Number of Fisher Scoring iterations: 4
glm.pred=rep(0,8378)
glm.pred[dec_predict>.5]=1
table(glm.pred,dec)
mean(glm.pred==dec)

dec_predict<-predict(actual_corr,full_data, type=response)

#Checking accuracy of prediction model
glm.pred=rep(0,8378)
glm.pred[dec_predict>.5]=1
table(glm.pred,dec)
mean(glm.pred==dec)
#> table(glm.pred,dec)
#        dec
#glm.pred    0    1
#       0 4033 1656
#       1  827 1862
#mean(glm.pred==dec)
#[1] 0.7036286
