#How well do you know yourself?

#This analysis aims to determine how much the values of what people said they were looking for in a partner correlate with the people they said 'yes' to

#First running the preliminary data cleaning code
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

#checking correlation
cor(yesdataimp$attr1_1,yesdataimp$"attr")

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

#Calculating the correlation between importance of same race and partner they said 'yes' to actually being of the same race

race_corr<-lm(imprace~samerace, data=yesdataimp)
summary(race_corr)

#Now we try a different approach where we compare the weighted sum of scores given by participants to partners with the decision variable

attach(full_data)
dataimp<-subset(full_data, select=c(dec, attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1,attr,sinc,intel,fun,amb,shar, imprace, samerace, int_corr))

interaction_lm<-glm(dec~attr1_1*attr+sinc1_1*sinc+intel1_1*intel+fun1_1*fun+amb1_1*amb+shar1_1*shar+imprace*samerace, data=dataimp, family=binomial)

 summary(interaction_lm)

#Call:
#glm(formula = dec ~ attr1_1 * attr + sinc1_1 * sinc + intel1_1 * 
#    intel + fun1_1 * fun + amb1_1 * amb + shar1_1 * shar + imprace * 
#    samerace, family = binomial)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.6476  -0.8103  -0.2857   0.8433   3.6642  

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.968716   1.696497  -2.339  0.01932 *  
#attr1_1          -0.042860   0.018789  -2.281  0.02254 *  
#attr              0.350835   0.044903   7.813 5.57e-15 ***
#sinc1_1           0.015181   0.024521   0.619  0.53586    
#sinc             -0.131765   0.054313  -2.426  0.01527 *  
#intel1_1         -0.003071   0.026118  -0.118  0.90641    
#intel            -0.055975   0.067834  -0.825  0.40927    
#fun1_1            0.015768   0.024471   0.644  0.51933    
#fun               0.259357   0.058607   4.425 9.63e-06 ***
#amb1_1           -0.062315   0.025493  -2.444  0.01451 *  
#amb              -0.268856   0.041228  -6.521 6.97e-11 ***
#shar1_1          -0.001703   0.021371  -0.080  0.93647    
#shar              0.175640   0.035252   4.982 6.28e-07 ***
#imprace          -0.116190   0.014237  -8.161 3.33e-16 ***
#samerace         -0.324787   0.102419  -3.171  0.00152 ** 
#attr1_1:attr      0.008794   0.001838   4.784 1.72e-06 ***
#sinc1_1:sinc      0.001946   0.002750   0.708  0.47913    
#intel1_1:intel    0.003985   0.002952   1.350  0.17694    
#fun1_1:fun        0.001531   0.002992   0.512  0.60880    
#amb1_1:amb        0.009891   0.003118   3.173  0.00151 ** 
#shar1_1:shar      0.007887   0.002647   2.980  0.00288 ** 
#imprace:samerace  0.089222   0.021311   4.187 2.83e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 9487.3  on 6931  degrees of freedom
#Residual deviance: 6923.7  on 6910  degrees of freedom
#  (1446 observations deleted due to missingness)
#AIC: 6967.7

#Number of Fisher Scoring iterations: 5

dec_predict<-predict(actual_corr,full_data, type="response")
dec<-dec[!is.na(dec_predict)]
dec_predict<-dec_predict[!is.na(dec_predict)]
glm.pred=rep(0,length(dec_predict))
glm.pred[dec_predict>.5]=1
> table(glm.pred,dec)
#        dec
#glm.pred    0    1
#       0 3114  929
#       1  812 2077
#> mean(glm.pred==dec)
#[1] 0.7488459
