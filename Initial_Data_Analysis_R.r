#Initial Data Analysis
#Want to figure out general percentiles of compatibility
#Also want to develop functions/routines to subset the data into training and test sets

#See Import_data_Script.r for the whole script to run when starting up R for the first time 
#and importing the data

#initially divide the data in to training and test data (20% size of the total data) before 
#we can resample it using the boostrap method
indices_list<-list()

for (w in 1:num_waves){
	for (g in 0:1){
		#Find the total number of rows in this category
		num_total<-length(full_data[full_data$wave==w & full_data$gender==g,1])

		#Sample the data indices to determine disjoint sets for training and test
		range_total<-1:num_total
		training_ind<-sample(range_total, floor(0.8*num_total), replace=FALSE,prob=NULL)
		test_ind<-range_total[-training_ind]

		#append the vector into the list
		name<-paste(w,paste(g,"training",sep="_"),sep="_")
		indices_list[[name]]<-training_ind
		name<-paste(w,paste(g,"test",sep="_"),sep="_")
		indices_list[[name]]<-test_ind
	}
}

#Looking at initial percentiles to answer questions like:
#1. What percentage of matches are between same ethnicity couples? (Aggregated by wave)
#2. What percentage of matches have the same field of study?
#3. What percentage of matches have the same or similar goals when participating in the event? 
#		Will look at squared differences.
#4. What percentage of matches have the same or similar going out habits (not necessarily on dates)? 
#		Hopefully this is a measure of social-ness
#5. What percentage of people change their mind before the end of the night about the people they would date?
#		Note that preferences can change throughout the night if they see people they would prefer more or less
#		If they do change their mind, what were some characteristics of the people they saw "in the meantime" 
#			before the end of the night?

#Question 1: What percentage of matches are between same ethnicity couples overall?
#For each wave we have to add up the number of matches. Have to be careful not to double
#count the people, so we pick one gender and then add up the matches for that
#Note that match is in quotes to not overload or get confused with the match function from R

#Already have a way to get the relevant wave and gender information--need to get the partner IDs (id and pid)

num_match_and_race<-0
total_match<-0
for (w in 1:num_waves){
	#unique list of female and male ids if we use id/partner which is better than iid since we can't get iid per row of partner
	#changes by wave
	female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
	male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)

	for(i in 1:length(female_id)){
		for (j in 1:length(male_id)){
			#female_race and male_race will each return a vector of hopefully the same race, 
			#so we pick the first value in that vector
			female_race<-full_data[full_data$wave==w & full_data$gender==0 & full_data$id==female_id[i],]$race[1]
			male_race<-full_data[full_data$wave==w & full_data$gender==1 & full_data$id==male_id[j],]$race[1]
			match_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"match"
			
			#check if the races match
			if(female_race==male_race & match_value==1){
				num_match_and_race<-num_match_and_race+1
			}
			#Whether or not races match
			if(match_value==1){
				total_match<-total_match+1
			}
		}				
	}	
}
#Don't need to break up proportions by wave but had to look at each wave separately
#since some of the id's and the partner id's are the same in each wave 
#i.e each wave has a female "id 1" and a male "id 1"
proportion_match_and_race<-num_match_and_race/total_match
num_match_and_race
total_match
proportion_match_and_race
#Of all the matches, around 40% of matches had the same race
#Output : 0.4101449

#What if there's a difference by race? (Total of 6 races)
#Want a break down of all the same race couples, how are they distributed across the races
break_down_race<-rep(0,6)
for (w in 1:num_waves){
	#unique list of female and male ids if we use id/partner which is better than iid since we can't get iid per row of partner
	#changes by wave
	female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
	male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
	
	for(i in 1:length(female_id)){
		for (j in 1:length(male_id)){
			#female_race and male_race will each return a vector of hopefully the same race, 
			#so we pick the first value in that vector
			female_race<-full_data[full_data$wave==w & full_data$gender==0 & full_data$id==female_id[i],]$race[1]
			male_race<-full_data[full_data$wave==w & full_data$gender==1 & full_data$id==male_id[j],]$race[1]
			match_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"match"
			
			#check if the races match; the races are the same so just use the female one
			if(female_race==male_race & match_value==1){
				break_down_race[female_race]<-break_down_race[female_race]+1
			}
		}				
	}	
}

#Reuse total_match from the other loop for any race equal to each other
total_match
#Output = 690

total_same_race_match<-sum(break_down_race)
total_same_race_match
#Output = 283

#% of total matches
black_african_american_proportion<-break_down_race[1]/total_match
black_african_american_proportion
#Output = 0.007246377

#% of same race matches
black_african_american_proportion_2<-break_down_race[1]/total_same_race_match
black_african_american_proportion_2
#Output = 0.01766784

#% of total matches
european_caucasian_american_proportion<-break_down_race[2]/total_match
european_caucasian_american_proportion
#Output = 0.3463768

#% of same race matches
european_caucasian_american_proportion_2<-break_down_race[2]/total_same_race_match
european_caucasian_american_proportion_2
#Output = 0.844523

#% of total matches
latino_hispanic_american_proportion<-break_down_race[3]/total_match
latino_hispanic_american_proportion
#Output = 0.008695652

#% of same race matches
latino_hispanic_american_proportion_2<-break_down_race[3]/total_same_race_match
latino_hispanic_american_proportion_2
#Output = 0.02120141

#% of total matches
asian_american_proportion<-break_down_race[4]/total_match
asian_american_proportion
#Output =  0.04492754

#% of same race matches
asian_american_proportion_2<-break_down_race[4]/total_same_race_match
asian_american_proportion_2
#Output = 0.1095406

#% of total matches = % of same race matches
native_american_proportion<-break_down_race[5]/total_match
native_american_proportion
#Output = 0

#% of total matches
other_race_proportion<-break_down_race[6]/total_match
other_race_proportion
#Output = 0.002898551

#% of same race matches
other_race_proportion_2<-break_down_race[6]/total_same_race_match
other_race_proportion_2
#Output = 0.007067138


#Question 2: What percentage of matches will have the same field of study?
#Loop to figure out proportions
num_same_field<-0
num_same_field_vector<-rep(0,18)
for(w in 1:num_waves){
	#Figure out the ids in each wave
	female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
	male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
	
	for(i in 1:length(female_id)){
		for (j in 1:length(male_id)){
			#female_field and male_field will each return a vector of hopefully the same field, 
			#so we pick the first value in that vector
			female_field<-full_data[full_data$wave==w & full_data$gender==0 & full_data$id==female_id[i],]$field_cd[1]
			male_field<-full_data[full_data$wave==w & full_data$gender==1 & full_data$id==male_id[j],]$field_cd[1]
			match_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"match"
			
			#check if the races match; the races are the same so just use the female one
			if(female_field==male_field & match_value==1){
				num_same_field<-num_same_field+1
				num_same_field_vector[female_field]<-num_same_field_vector[female_field]+1
			}
		}				
	}	
}

#Proportion of matches that have the same field for both males and females
#total_match is the overall number of matches
same_field_match_prop<-num_same_field/total_match
same_field_match_prop
#Output = 0.1594203

#breakdown of field matches by field 
total_same_field_match<-sum(num_same_field_vector)
num_same_field_vector
#Highest values are field_cd 1, field_cd 8, and field_cd 10
#which correspond to Law, Business/Econ/Finance , and  Biological Sciences/Chemistry/Physics
#These proportions may be skewed since Columbia is primarily known for its Law, Business schools
#as well as a strong programs in science.


#######Activity vector error calculation######
#Want to calculate the difference vector between two participant's interests in different activities
#then we can use the 2-norm, squared or the 1-norm between the two vectors so that it will be a measure
#of the difference in two people's interests.  The hypothesis is, if two people have similar interests,
#then they are more likely to match with each other and have followup conversations

#The ratings for their interests are in the "Time 1" or signup time so we can consider those views
#to be independent of the people that they meet (a guy may pretend to love cats if he's talking to
#an attractive woman who likes cats as a way to pique her interest as well)

#Can use the "which" to figure out the column number from column name
#The first column name in the activity vector is "sports"
first_col<-which(colnames(full_data)=="sports")
#The last column name int he activity vector is "yoga"
last_col<-which(colnames(full_data)=="yoga")

#For each wave, for each female, if she matches with a male, then we want to look at 
#the norm of the difference between their activity vectors. 

#Have to make sure to look at the matches just for one gender so we don't double
#count the data

#Making a matrix of the vectors and the match decision then fit a linear model using least squares
#Use the total_interactions calculation from the data import to initialize the matrix
#There are 17 different activities for each person so the number of columns is 34
activity_matrix<-matrix(rep(0,34*total_interactions),ncol=34,nrow=total_interactions)
result<-rep(0,total_interactions)
counter<-1
for (w in 1:num_waves){
	#Figure out the ids in each wave
	female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
	male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
	
	#Loop through all the pairs in the wave
	for (i in 1:length(female_id)){
		#Assign the activity vector for the female
		f_vector<-full_data[full_data$wave==w & full_data$gender==0 & 
			full_data$id==female_id[i],first_col:last_col][1,]
		for (j in 1:length(male_id)){
			#Assign the activity vector for the male
			m_vector<-full_data[full_data$wave==w & full_data$gender==1 & 
				full_data$id==male_id[j],first_col:last_col][1,]
			
			#Determine whether or not there's a match and what the decisions were
			match_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"match"
			female_dec_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"dec"
			male_dec_value<-full_data[full_data$wave==w & full_data$gender==1 & 
				full_data$id==male_id[j] & full_data$partner==female_id[i], ]$"dec"	
			
			#Append the activity vectors
			activity_matrix[counter,1:17]<-t(f_vector)
			activity_matrix[counter,18:34]<-t(m_vector)
			result[counter]<-match_value
			counter<-counter+1
			
			# #Take the difference between the two vectors
			# difference_vector<-abs(f_vector-m_vector)
			
			# #Calculate the 1-norm of the difference_vector
			# one_norm<-sum(difference_vector)
			
			# #Calculate the 2-norm of the difference vector
			# two_norm<-sqrt(sum(difference_vector^2))
			
		}
	}	
}

#Now that we have the data, we want to find a least squares solution to the model
#To use lm we need to have a data frame--thankfully simple to convert
lm_fit<-lm(result~.,data=data.frame(activity_matrix))
summary(lm_fit)

# Call:
# lm(formula = result ~ ., data = data.frame(activity_matrix))

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -0.3374 -0.1931 -0.1476 -0.0836  1.0252 

# Coefficients:
              # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.0113703  0.0647975  -0.175  0.86072    
# X1           0.0054704  0.0029655   1.845  0.06516 .  
# X2          -0.0076275  0.0027297  -2.794  0.00523 ** 
# X3          -0.0034318  0.0028146  -1.219  0.22281    
# X4          -0.0015785  0.0043810  -0.360  0.71863    
# X5          -0.0013749  0.0061779  -0.223  0.82389    
# X6           0.0101790  0.0053005   1.920  0.05488 .  
# X7          -0.0001508  0.0026907  -0.056  0.95532    
# X8           0.0022679  0.0026681   0.850  0.39536    
# X9           0.0094641  0.0025049   3.778  0.00016 ***
# X10          0.0006501  0.0032385   0.201  0.84091    
# X11          0.0064402  0.0029041   2.218  0.02664 *  
# X12          0.0023265  0.0038183   0.609  0.54236    
# X13         -0.0127717  0.0046052  -2.773  0.00557 ** 
# X14          0.0124699  0.0044441   2.806  0.00504 ** 
# X15         -0.0072423  0.0046838  -1.546  0.12213    
# X16         -0.0034914  0.0030029  -1.163  0.24503    
# X17          0.0020476  0.0023193   0.883  0.37736    
# X18          0.0025646  0.0030094   0.852  0.39416    
# X19         -0.0002179  0.0025596  -0.085  0.93217    
# X20          0.0019555  0.0026971   0.725  0.46848    
# X21          0.0093401  0.0036152   2.584  0.00981 ** 
# X22         -0.0176741  0.0058068  -3.044  0.00235 ** 
# X23          0.0115802  0.0050767   2.281  0.02260 *  
# X24          0.0018752  0.0024852   0.755  0.45058    
# X25         -0.0007061  0.0024549  -0.288  0.77364    
# X26          0.0059778  0.0025464   2.348  0.01894 *  
# X27          0.0085530  0.0030643   2.791  0.00528 ** 
# X28         -0.0015935  0.0031107  -0.512  0.60850    
# X29         -0.0039732  0.0037038  -1.073  0.28345    
# X30         -0.0078766  0.0041468  -1.899  0.05758 .  
# X31         -0.0007684  0.0036884  -0.208  0.83498    
# X32          0.0060899  0.0041221   1.477  0.13965    
# X33         -0.0025406  0.0029085  -0.874  0.38243    
# X34          0.0042254  0.0024585   1.719  0.08575 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3671 on 4075 degrees of freedom
  # (79 observations deleted due to missingness)
# Multiple R-squared:  0.02648,   Adjusted R-squared:  0.01836 
# F-statistic:  3.26 on 34 and 4075 DF,  p-value: 6.58e-10

#Significant predictors at 0.05 are:
#X2, X9, X13, X14, X21, X22, X23, X26, X27 but there are many other correlated variables in the model

#Also want to use forward and backward selection and the AIC to determine which predictors should be in the model
library(MASS)
step_lm<-stepAIC(lm_fit,direction="both")

#Partial output:
# result ~ X1 + X2 + X6 + X9 + X11 + X13 + X14 + X15 + X18 + X21 + 
    # X22 + X23 + X26 + X27 + X30 + X32 + X33 + X34
#The coefficients for these variables are
#Female:
	#Sports, TV Sports, Art, Dancing/Clubbing, Watching TV, Theater, Movies, Going to concerts
#Male:
	#Sports, Dining Out, Museums, Art, Dancing/Clubbing, Reading, Movies, Music,Shopping, Yoga
	
	
############################
#Want to investigate response rates for surveys
############################
#Number of questions for each survey is determined from the key

#Initial survey
initial_survey_num_questions<-48
#for each person and for field in the intiial survey we want to know
#if they answered the question or not (not NA)
#The correlated columns are 34 to 81
#Pull out the relevant columns of the matrix for the initial survey
initial_survey_response_matrix<-full_data[,34:81]
#Want to determine the number of questions that a person answered
#Returns a vector of response rates for the first survey
first_survey_na_num<-rowSums(is.na(initial_survey_response_matrix))
first_survey_response_rate<-(initial_survey_num_questions-first_survey_na_num)/initial_survey_num_questions

#Want to determine the number of questions that females answered
#Define a new subset of the data for readability
first_survey_female_response_matrix<-initial_survey_response_matrix[full_data$gender==0,]
#Want to determine the number of questions that a female answered
#Returns a vector of response rates for the first survey that was done by females
first_survey_na_num_female<-rowSums(is.na(first_survey_female_response_matrix))
first_survey_female_response_rate<-(initial_survey_num_questions-first_survey_na_num_female)/initial_survey_num_questions

#Want to determine the number of questions that males answered
#Define a new subset of the data for readability
first_survey_male_response_matrix<-initial_survey_response_matrix[full_data$gender==1,]
#Want to determine the number of questions that a male answered
#Returns a vector of response rates for the first survey that was done by males
first_survey_na_num_male<-rowSums(is.na(first_survey_male_response_matrix))
first_survey_male_response_rate<-(initial_survey_num_questions-first_survey_na_num_male)/initial_survey_num_questions

#Plot a histogram of the distribution of the response rates for the first survey for both genders
hist(first_survey_response_rate,main = "Response rates for initial survey (both genders)",xlab="Response Rate",ylab="Number of participants")

#Plot a histogram of the distribution of the response rates for the first survey for females only 
hist(first_survey_female_response_rate,main = "Response rates for initial survey (female only)",xlab="Response Rate",ylab="Number of participants")

#Plot a histogram of the distribution of the response rates for the first survey for males only 
hist(first_survey_male_response_rate,main = "Response rates for initial survey (male only)",xlab="Response Rate",ylab="Number of participants")

###################################################################
#Halfway through survey (not including scorecard)
halfway_survey_num_questions<-11
#Pull out the relevant columns of the matrix for the survey that's halfway through 
halfway_survey_response_matrix<-full_data[,109:119]
#Want to determine the number of questions that a person answered
#Returns a vector of response rates for the survey halfway through the event
halfway_survey_na_num<-rowSums(is.na(halfway_survey_response_matrix))
halfway_survey_response_rate<-(halfway_survey_num_questions-halfway_survey_na_num)/halfway_survey_num_questions

#Want to determine the number of questions that females answered
#Define a new subset of the data for readability
halfway_survey_female_response_matrix<-halfway_survey_response_matrix[full_data$gender==0,]
#Want to determine the number of questions that a female answered
#Returns a vector of response rates for the halfway survey that was done by females
halfway_survey_na_num_female<-rowSums(is.na(halfway_survey_female_response_matrix))
halfway_survey_female_response_rate<-(halfway_survey_num_questions-halfway_survey_na_num_female)/halfway_survey_num_questions

#Want to determine the number of questions that males answered
#Define a new subset of the data for readability
halfway_survey_male_response_matrix<-halfway_survey_response_matrix[full_data$gender==1,]
#Want to determine the number of questions that a male answered
#Returns a vector of response rates for the first survey that was done by males
halfway_survey_na_num_male<-rowSums(is.na(halfway_survey_male_response_matrix))
halfway_survey_male_response_rate<-(halfway_survey_num_questions-halfway_survey_na_num_male)/halfway_survey_num_questions

#Plot a histogram of the distribution of the response rates for the survey halfway through the speed dating event for both genders
hist(halfway_survey_response_rate,main = "Response rates for survey halfway through event (both genders)",xlab="Response Rate",ylab="Number of participants")

#Plot a histogram of the distribution of the response rates for the survey halfway through the speed dating event for females only
hist(halfway_survey_female_response_rate,main = "Response rates for survey halfway through event (females_only)",xlab="Response Rate",ylab="Number of participants")

#Plot a histogram of the distribution of the response rates for the survey halfway through the speed dating event for males only
hist(halfway_survey_male_response_rate,main = "Response rates for survey halfway through event (males_only)",xlab="Response Rate",ylab="Number of participants")
###################################################################
#First followup to get their matches survey (day after their speed dating)
first_followup_num_questions<-37
#Pull out the relevant columns of the matrix for the first followup survey 
first_followup_survey_response_matrix<-full_data[,120:156]
#Want to determine the number of questions that a person answered
#Returns a vector of response rates for the first follow up survey
first_followup_survey_na_num<-rowSums(is.na(first_followup_survey_response_matrix))
first_followup_survey_response_rate<-(first_followup_num_questions-first_followup_survey_na_num)/first_followup_num_questions

#Want to determine the number of questions that females answered
#Define a new subset of the data for readability
first_followup_survey_female_response_matrix<-first_followup_survey_response_matrix[full_data$gender==0,]
#Want to determine the number of questions that a female answered
#Returns a vector of response rates for the first followup survey that was done by females
first_followup_survey_na_num_female<-rowSums(is.na(first_followup_survey_female_response_matrix))
first_followup_survey_female_response_rate<-(first_followup_num_questions-first_followup_survey_na_num_female)/first_followup_num_questions

#Want to determine the number of questions that males answered
#Define a new subset of the data for readability
first_followup_survey_male_response_matrix<-first_followup_survey_response_matrix[full_data$gender==1,]
#Want to determine the number of questions that a male answered
#Returns a vector of response rates for the first survey that was done by males
first_followup_survey_na_num_male<-rowSums(is.na(first_followup_survey_male_response_matrix))
first_followup_survey_male_response_rate<-(first_followup_num_questions-first_followup_survey_na_num_male)/first_followup_num_questions

#Note that the histogram is the response rates for both genders
#Plot a histogram of the distribution of the response rates for the first followup survey
hist(first_followup_survey_response_rate,main = "Response rates for first followup survey (both genders)",xlab="Response Rate",ylab="Number of participants")

#Note that the histogram is the response rates for females only
#Plot a histogram of the distribution of the response rates for the first followup survey
hist(first_followup_survey_female_response_rate,main = "Response rates for first followup survey (females only)",xlab="Response Rate",ylab="Number of participants")

#Note that the histogram is the response rates for males only
#Plot a histogram of the distribution of the response rates for the first followup survey
hist(first_followup_survey_male_response_rate,main = "Response rates for first followup survey (males only)",xlab="Response Rate",ylab="Number of participants")
###################################################################
#Second follow up survey that asks whether or not they met up with their matches
#Sent 3-4 weeks after their matches
second_followup_num_questions<-39
#Pull out the relevant columns of the matrix for the first followup survey 
second_followup_survey_response_matrix<-full_data[,157:195]
#Want to determine the number of questions that a person answered
#Returns a vector of response rates for the second follow up survey
second_followup_survey_na_num<-rowSums(is.na(second_followup_survey_response_matrix))
second_followup_survey_response_rate<-(second_followup_num_questions-second_followup_survey_na_num)/second_followup_num_questions

#Want to determine the number of questions that females answered
#Define a new subset of the data for readability
second_followup_survey_female_response_matrix<-second_followup_survey_response_matrix[full_data$gender==0,]
#Want to determine the number of questions that a female answered
#Returns a vector of response rates for the second followup survey that was done by females
second_followup_survey_na_num_female<-rowSums(is.na(second_followup_survey_female_response_matrix))
second_followup_survey_female_response_rate<-(second_followup_num_questions-second_followup_survey_na_num_female)/second_followup_num_questions

#Want to determine the number of questions that males answered
#Define a new subset of the data for readability
second_followup_survey_male_response_matrix<-second_followup_survey_response_matrix[full_data$gender==1,]
#Want to determine the number of questions that a male answered
#Returns a vector of response rates for the second followup survey that was done by males
second_followup_survey_na_num_male<-rowSums(is.na(second_followup_survey_male_response_matrix))
second_followup_survey_male_response_rate<-(second_followup_num_questions-second_followup_survey_na_num_male)/second_followup_num_questions

#Note that the histogram is the response rates for both genders
#Plot a histogram of the distribution of the response rates for the second followup survey
hist(second_followup_survey_response_rate,main = "Response rates for second followup survey (both genders)",xlab="Response Rate",ylab="Number of participants")

#Note that the histogram is the response rates for females only
#Plot a histogram of the distribution of the response rates for the second followup survey
hist(second_followup_survey_female_response_rate,main = "Response rates for second followup survey (females only)",xlab="Response Rate",ylab="Number of participants")

#Note that the histogram is the response rates for males only
#Plot a histogram of the distribution of the response rates for the second followup survey
hist(second_followup_survey_male_response_rate,main = "Response rates for second followup survey (males only)",xlab="Response Rate",ylab="Number of participants")
###################################################################
#What are the questions that have the lowest response rates?

#Include interaction effect with response rate and model coefficients to predict match or decision, i.e. figure out how to include/exclude the data points with missing variables

#Need to make sure that when we make linear models that we don't exclude the data that we don't want to
#For example, if we are using variables 1-5 to fit a regression but a data point doesn't have variable 6, then we would still want to fit or predict for this point.
#Looking back at the activity matrix example since I took out the routine that 
#converts NA values to be 0, there are a total of 79 observations that were not included
#in the model--looks like 59 from the females and 20 from the males.
#Thus I think that observations are deleted if the data is not all there.  
#Either we need to have smaller but complete models or if we include more variables,
#maybe include an indicator function so that the rest of the data can be used.

###################################################################
#Also want to implement KNN (with k=3, 5, 7) to see if that is a good algorithm for prediction
library(class)

#First try KNN with k=3 and all the variables
#the problem is, no missing variables are allowed!
#Still have to use the cleaning techniques

#Make a copy of the original data set
orignal_full_data<-full_data

#Run the cleaning routine that will clean the full_data dataset
full_data<-reassign_all_data(full_data)
full_data<-clean_NA_data(full_data)
#This runs with errors; all the NA's are fixed in the functions but may have too much data
#Also need to exclude the categorical variables
non_cat_variables<-colnames(full_data) #all column names
#Define by hand the ones that are non-numeric and not the match variable
cat_variables<-c("undergra","career","income","from","zipcode","mn_sat","tuition","field","match")
non_cat_variables<-non_cat_variables[!non_cat_variables %in% cat_variables]

#Run the KNN model with 3 neighbors
knn_3_model<-knn.cv(full_data[non_cat_variables],full_data$"match",3)
summary(knn_3_model)
 # 0    1 
# 7585  793 
mis_classified<-sum(abs(as.numeric(knn_3_model)-full_data$"match"))
mis_classified #Output is 7791! Wow this method is no good with only 3 neighbors

#Try with 5 neighbors
knn_5_model<-knn.cv(full_data[non_cat_variables],full_data$"match",5)
mis_classified<-sum(abs(as.numeric(knn_5_model)-full_data$"match"))
mis_classified #Output is 7562
#Number of misclassifications still high but hopefully adding more neighbors to make model more flexible

mis_classified_vector<-rep(0,25)
for (i in 1:25){
	model_fit<-knn.cv(full_data[non_cat_variables],full_data$"match",3+i)
	mis_classified<-sum(abs(as.numeric(model_fit)-full_data$"match"))
	mis_classified_vector[i]<-mis_classified
}


#Also need to implement some sort of resampling technique for training the model and testing it on new data since we only have ~8k rows of information








