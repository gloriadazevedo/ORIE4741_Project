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

#Bootstrap function that creates training data and test data from the data, given a wave and gender
#Note that we can subset data as so: full_data[full_data$wave==1&full_data$gender==0,][1,]
#This will return the first row of the females in wave 1.
#For each individual in the wave-gender pair they see all of the people in the opposite gender in that wave 
#so in general for each wave-gender pair, there are num_males*num_females number of rows.
#However, we have on exception, in Wave 5 where there's a note that they're all undergrads, we have different
#numbers of rows between the wave 5 females and males.  Thus we predict that one of the females did not 
#have her data recorded for the 10 males.
bootstrap_wave_gender<-function(w,g,type=c("training","test")){
	#Determine the number of people in this wave_gender pair
	#Need to get the list of training or test vertices from the indices_list
	#Need to define the name that we use to pull from the list
	name<-paste(w,paste(g,type,sep="_"),sep="_")
	indices_sample<-indices_list[[name]]
	
	#For the best bootstrapping technique, we need to resample the data the same number of times as the number we have
	#First we need to divide the data into training and test
	range_total<-1:length(indices_sample)
	sampling<-sample(range_total, length(indices_sample), replace=TRUE,prob=NULL)
	
	return_data<-full_data[full_data$wave==w & full_data$gender==g,][indices_sample[sampling]]
	
	return (return_data)
}

#Test function
wave_1_gender_0_train<-bootstrap_wave_gender(1,0,"training")
wave_1_gender_0_test<-bootstrap_wave_gender(1,0,"test")

#To ensure a "match" where two people like each other, each person has to fill out a "yes" for the ID of the other person
#on their scorecard within that wave.
#i.e. for persons 1 and 2 to like each other, person 1 has to circle "yes" for person 2 and person 2 has to circle "yes" for person 1
#The person filling it out has their id within the wave in column "id" while their partner is in "pid"
#Creating a function that, when given a wave, and the ids, determine if it's a match, the male said they would date and the 
#female did not, and if the male said they would not and female said that they would.

#Turns out there's a match column that says whether or not the two people have matched
match_results<-function(w,p1,p2,data_source){

	#figure out gender--only have to do this for one and then the other one is expected to the the other gender.
	p1_gender<-data_source[data_source$wave==w & data_source$id==p1 & !is.na(data_source$iid) & data_source$pid==p2,]$gender

	p1_decision<-data_source[data_source$wave==w & data_source$id==p1 & !is.na(data_source$iid) & data_source$pid==p2,]$dec
	p2_decision<-data_source[data_source$wave==w & data_source$id==p2 & !is.na(data_source$iid) & data_source$pid==p1,]$dec
	
	both_match<-p1_decision & p2_decision
	
	if(p1_gender==0){
		female_decision<-p1_decision
		male_decision<-p2_decision
	}else{
		male_decision<-p1_decision
		female_decision<-p2_decision
	}
	
	return_list<-list("both_match"=both_match,"female_decision"=female_decision,"male_decision"=male_decision)
	
	return (return_list)
}

#Test function
return_list<-match_results(1,1,11,full_data)
return_list$both_match #Returns False or 0 since both of them did not put true
return_list$female_decision #Returns True or 1
return_list$male_decision #Returns False or 0


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
	
	for (i in 1:length(female_id)){
		#Assign the activity vector for the female
		f_vector<-full_data[full_data$wave==w & full_data$gender==0 & 
			full_data$id==female_id[i],first_col:last_col][1,]
		for (j in 1:length(male_id)){
			#Assign the activity vector for the male
			m_vector<-full_data[full_data$wave==w & full_data$gender==1 & 
				full_data$id==male_id[j],first_col:last_col][1,]
			
			#Determine whether or ot there's a match
			match_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"match"
			
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

#Also want to use forward and backward selection and the AIC to determine which predictors should be in the model
library(MASS)
step_lm<-stepAIC(lm_fit,direction="both")

#Partial output:
# result ~ X1 + X2 + X6 + X9 + X11 + X13 + X14 + X15 + X18 + X21 + 
    # X22 + X23 + X26 + X27 + X30 + X32 + X33 + X34