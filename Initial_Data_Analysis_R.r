#Initial Data Analysis
#Want to figure out general percentiles of compatibility
#Also want to develop functions/routines to subset the data into training and test sets

#Import data
full_data<-read.table("data_excel.csv",header=TRUE,sep=",")
#When importing we ran into a few issues such as some lines not having the correct number of elements, so added fill=TRUE for the blanks.

#Data editing
#For ease of use and not using the Fill in feature which seems to be adding NA's everywhere, 
#we are going to edit the data by hand in Excel just so there are no blanks in the csv.
#The fields that are edited are documented in another .txt (Data_Editing.txt) file

#When creating the test and training data sets, we want to separate them out by wave, so that any inter-wave effects are gone
#We also note that we want to subdivide the test and training set by male/female so we can see if there are different variables
#that are strong predictors for men and women.
#Since each wave only has between 50 and 200 data points, median of 280, bootstrapping may be the best option to resample the
#data and have a better idea of the parameters of the model and their sensitivity.  Also we note that there are multiple 
#waves on each day, so it could be beneficial to combine the waves and treat them on a "per-day" basis as we hypothesize 
#that there is no overlap between the two sessions in a day.  In other words, we assume that the sets of people attending
#each session in one day is mutually exclusive.

#Adding the actual day number to the data
#First we create a function, then we're going to copy over the wave column into the new date column,
#then we will peform the function on the new date column and overwrite it.
#w is an integer between 1 and 21, inclusive
check_date<-function(w){
	d=0
	if(w==1){d=1}
	else if(w==2){d=2}
	else if(w==3 |w==4){d=3}
	else if(w==5){d=4}
	else if(w==6|w==7){d=5}
	else if(w==8 | w==9){d=6}
	else if(w==10 |w==11){d=7}
	else if(w==12){d=8}
	else if(w==13|w==14){d=9}
	else if(w==15){d=10}
	else if(w==16|w==17){d=11}
	else if(w==18|w==19){d=12}
	else if(w==20|w==21){d=13}
	
	return (d)
}

#Test the function
test_vector<-1:21
for (i in 1:21){
	test_vector[i]<-check_date(i)
}

#Create a temporary vector of the wave to date
date_vector<-1:length(full_data$wave)
for(i in 1:length(date_vector)){
	date_vector[i]<-check_date(full_data$wave[i])
}
#Assign the temporary vector to a new column in the data
full_data$day_num<-date_vector

#General statistics
#How many waves
num_waves<-max(full_data[!is.na(full_data$wave),]$wave)

#Frequency of each wave, total and by gender
wave_freq<-table(full_data$wave)
wave_gender_freq<-table(full_data$wave,full_data$gender)

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
num_match_and_race<-0
total_match<-0
for (w in 1:num_waves){
	#Use gender==0
	g<-0
	female_race<-full_data[full_data$wave==w & full_data$gender==g,]$race
	male_race<-full_data[full_data$wave==w & full_data$gender==!g,]$race
	match_1<-full_data[full_data$wave==w & full_data$gender==g,]$"match"
	for(i in 1:length(female_race)){
		if((female_race[i]==male_race[i]) & (match_1[i]==1) & female_race[i]!="NA" & male_race[i]!="NA"){
			num_match_and_race<-num_match_and_race+1
		}
		if(match_1[i]==1 & female_race[i]!="NA" & male_race[i]!="NA"){
			total_match<-total_match+1
		}
	}
	
}
#Don't need to break up by wave
proportion_match_and_race<-num_match_and_race/total_match
proportion_match_and_race
#Output : 0.3685504

#What if there's a difference by race? (Total of 6 races)
num_match_and_race<-1:6
total_match<-1:6
for (w in num_waves){
	#Use gender==0
	g<-0
	female_race<-full_data[full_data$wave==w & full_data$gender==g,]$race
	male_race<-full_data[full_data$wave==w & full_data$gender==!g,]$race
	match_1<-full_data[full_data$wave==w & full_data$gender==g,]$"match"
	for(r in 1:6){
		#Need to reset the value in the output vector
		num_match_and_race[r]<-0
		total_match[r]<-0
		for(i in 1:length(female_race)){
			if(female_race[i]==r & male_race[i]==r &match_1[i]==1 & female_race[i]!="NA" & male_race[i]!="NA"){
				num_match_and_race[r]<-num_match_and_race[r]+1
			}
			if(match_1[i]==1 & female_race[i]!="NA" & male_race[i]!="NA"){
				total_match[r]<-total_match[r]+1
			}
		}
	}
}
#check proportions--don't see a strong link between race and matching
black_african_american_proportion<-if(total_match[1]>0){num_match_and_race[1]/total_match[1]}
#Output = 0

european_caucasian_american_proportion<-if(total_match[2]>0){num_match_and_race[2]/total_match[2]}
#Output = 1/6

latino_hispanic_american_proportion<-if(total_match[3]>0){num_match_and_race[3]/total_match[3]}
#Output = 0

asian_american_proportion<-if(total_match[4]>0){num_match_and_race[4]/total_match[4]}
#Output = 1/12

native_american_proportion<-if(total_match[5]>0){num_match_and_race[5]/total_match[5]}
#Output = 0

other_race_proportion<-if(total_match[6]>0){num_match_and_race[6]/total_match[6]}
#Output = 0