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
sapply(full_data, function(x) sum(is.na(x)))

dataimp<-subset(full_data, select=c(dec,attr,sinc,intel,fun,amb,shar,like,prob,met,goal,date,go_out,exphappy))
 install.packages("leaps")
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
