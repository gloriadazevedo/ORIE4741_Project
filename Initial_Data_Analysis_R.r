#Initial Data Analysis
#Want to figure out general percentiles of compatibility
#Also want to develop functions/routines to subset the data into training and test sets

#Import data
full_data<-read.table("Speed Dating Data.csv",header=TRUE,sep=",",fill=TRUE)
#When importing we ran into a few issues such as some lines not having the correct number of elements, so added fill=TRUE for the blanks.

#When creating the test and training data sets, we want to separate them out by wave, so that any inter-wave effects are gone
#We also note that we want to subdivide the test and training set by male/female so we can see if there are different variables
#that are strong predictors for men and women.
#Since each wave only has between 50 and 200 data points, median of 280, bootstrapping may be the best option to resample the
#data and have a better idea of the parameters of the model and their sensitivity.  Also we note that there are multiple 
#waves on each day, so it could be beneficial to combine the waves and treat them on a "per-day" basis as we hypothesize 
#that there is no overlap between the two sessions in a day.  In other words, we assume that the sets of people attending
#each session in one day is mutually exclusive.

#Bootstrap function that creates training data and test data from the data, given a wave and gender
#Note that we can subset data as so: full_data[full_data$wave==1&full_data$gender==0,][1,]
#This will return the first row of the females in wave 1.
#For each individual in the wave-gender pair they see all of the people in the opposite gender in that wave 
#so in general for each wave-gender pair, there are num_males*num_females number of rows.
#However, we have on exception, in Wave 5 where there's a note that they're all undergrads, we have different
#numbers of rows between the wave 5 females and males.  Thus we predict that one of the females did not 
#have her data recorded for the 10 males.
bootstrap_wave_gender<-function(w,g,data_source){
	#Determine the number of people in this wave_gender pair
	#Also need to make sure we don't pick up any of the filled data with NA's for the iid
	num_total<-length(data_source[data_source$wave==w & data_source$gender==g & !is.na(data_source$iid),1])
	
	#For the best bootstrapping technique, we need to resample the data the same number of times as the number we have
	#Use 20% of the training data size for the test data
	range_total<-1:num_total
	training_ind<-sample(range_total, num_total, replace=TRUE,prob=NULL)
	test_ind<-sample(range_total,floor(num_total/5),replace=TRUE,prob=NULL)
	
	test_data<-data_source[data_source$wave==w & data_source$gender==g & !is.na(data_source$iid),][test_ind,]
	train_data<-data_source[data_source$wave==w & data_source$gender==g & !is.na(data_source$iid),][training_ind,]
	
	#Create a list to return the values
	return_list<-list('test' = test_data,'train' = train_data)
	
	return (return_list)
}

#Test function
return_list<-bootstrap_wave_gender(1,0,full_data)
wave_1_gender_0_test<-return_list$test
wave_1_gender_0_train<-return_list$train

#To ensure a "match" where two people like each other, each person has to fill out a "yes" for the ID of the other person
#on their scorecard within that wave.
#i.e. for persons 1 and 2 to like each other, person 1 has to circle "yes" for person 2 and person 2 has to circle "yes" for person 1
#The person filling it out has their id within the wave in column "id" while their partner is in "pid"
#Creating a function that, when given a wave, and the ids, determine if it's a match, the male said they would date and the 
#female did not, and if the male said they would not and female said that they would.
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

