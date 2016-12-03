#Trying to predict which predictors are most important in determining a person's final decision by finding the most important variables using subset selection.
#In our case we have used all-subset selection where every possible combination of predictors is considered for a given number of maximum predictors allowed. The output shows the best subset of predictors for a given max number of permitted predictors in the model.

library(gdata)
setwd("/Users/pihu_yadav/Downloads/ORIE4741_Project-master")
full_data<-read.table("data_excel.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

options(na.action = "na.exclude") 

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




#Performing mixed selection to determine the most important factors in determining the decision of the individual
#
full_data_non_na<-sapply(full_data, function(x) sum(is.na(x)))

dataimp<-subset(full_data_non_na, select=c(dec,attr,sinc,intel,fun,amb,shar,like,prob,met,goal,date,go_out,exphappy))
# install.packages("leaps")
library(leaps)
attach(dataimp)
regsubsets.out <-
    regsubsets(dec~attr+sinc+intel+fun+amb+shar+like+prob+met+goal+date+go_out+exphappy,
               data = dataimp,
               nbest = 1,       # 1 best model for each number of predictors
               nvmax = NULL,    # NULL for no limit on number of variables
               force.in = NULL, force.out = NULL,
               method = "exhaustive")
summary(regsubsets.out)
#Subset selection object
#Call: regsubsets.formula(dec ~ attr + sinc + intel + fun + amb + shar + 
#    like + prob + met + goal + date + go_out + exphappy, data = dataimp, 
#    nbest = 1, nvmax = NULL, force.in = NULL, force.out = NULL, 
#    method = "exhaustive")
#13 Variables  (and intercept)
#         Forced in Forced out
#attr         FALSE      FALSE
#sinc         FALSE      FALSE
#intel        FALSE      FALSE
#fun          FALSE      FALSE
#amb          FALSE      FALSE
#shar         FALSE      FALSE
#like         FALSE      FALSE
#prob         FALSE      FALSE
#met          FALSE      FALSE
#goal         FALSE      FALSE
#date         FALSE      FALSE
#go_out       FALSE      FALSE
#exphappy     FALSE      FALSE
#1 subsets of each size up to 13
#Selection Algorithm: exhaustive
#          attr sinc intel fun amb shar like prob met goal date go_out exphappy
#1  ( 1 )  " "  " "  " "   " " " " " "  "*"  " "  " " " "  " "  " "    " "     
#2  ( 1 )  "*"  " "  " "   " " " " " "  "*"  " "  " " " "  " "  " "    " "     
#3  ( 1 )  "*"  "*"  " "   " " " " " "  "*"  " "  " " " "  " "  " "    " "     
#4  ( 1 )  "*"  "*"  " "   " " " " " "  "*"  "*"  " " " "  " "  " "    " "     
#5  ( 1 )  "*"  "*"  " "   " " "*" " "  "*"  "*"  " " " "  " "  " "    " "     
#6  ( 1 )  "*"  "*"  " "   " " "*" "*"  "*"  "*"  " " " "  " "  " "    " "     
#7  ( 1 )  "*"  "*"  " "   "*" "*" "*"  "*"  "*"  " " " "  " "  " "    " "     
#8  ( 1 )  "*"  "*"  " "   "*" "*" "*"  "*"  "*"  " " " "  " "  " "    "*"     
#9  ( 1 )  "*"  "*"  " "   "*" "*" "*"  "*"  "*"  " " " "  " "  "*"    "*"     
#10  ( 1 ) "*"  "*"  " "   "*" "*" "*"  "*"  "*"  " " " "  "*"  "*"    "*"     
#11  ( 1 ) "*"  "*"  " "   "*" "*" "*"  "*"  "*"  " " "*"  "*"  "*"    "*"     
#12  ( 1 ) "*"  "*"  " "   "*" "*" "*"  "*"  "*"  "*" "*"  "*"  "*"    "*"     
#13  ( 1 ) "*"  "*"  "*"   "*" "*" "*"  "*"  "*"  "*" "*"  "*"  "*"    "*"   
plot(regsubsets.out)
#Output for this can be found in BestModelSubsetPlot.pdf
#From the plot we see that the best model (lowest BIC) is the one with nine variables
#Coefficients of the model with nine variables are below
coef(regsubsets.out, 9)
#(Intercept)        attr        sinc         fun         amb        shar 
#-0.48931702  0.06591967 -0.03003476  0.01641642 -0.02306307  0.01451100 
#       like        prob      go_out    exphappy 
# 0.08065556  0.02311936  0.01392134  0.01022608 

#Now we need to calculate the misclassification rate for each of the models 
#so that we have a standard measure of predicting power for alll models.
#Basically estimating the training error rate
num_misclassifications<-rep(0,13)
for(i in 1:13){
	coef_i<-coef(regsubsets.out,id=i)
	#Returns a bunch of real values almost between 0 and 1
	#Use a 0.5 threshold to determine 1 or 0
	pred<-as.matrix(dataimp[,names(coef_i[-1])])%*%as.matrix(coef_i[-1])+coef_i[1]
	
	#Iterate through and determine whether it's a yes or no
	sum_predictions<-0
	for(j in 1:length(pred)){
		if(!is.na(pred[j])){
			result<-0
			if(pred[j]>0.5){
				result<-1
				}
			sum_predictions<-sum_predictions+abs(result-dataimp[j,]$dec)
			}
		}
	
	num_misclassifications[i]<-sum_predictions
}
num_misclassifications
 #2118 1998 1930 1872 1775 1632 1623 1600 1586 1571 1568 1531 1527
 plot(num_misclassifications)

 #Find misclassification rate for size 9
 num_misclassifications[9]/sum(!is.na(pred))

#Get all BIC values for the top 13 models:
summary(regsubsets.out)$bic
# -2090.250 -2422.045 -2489.608 -2577.021 -2595.057 -2618.180 -2628.115
 # [8] -2632.345 -2633.231 -2629.828 -2624.306 -2615.868 -2607.113
summary(summary(regsubsets.out)$bic)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # -2633   -2628   -2616   -2551   -2577   -2090 
