#R file just to have all the functions in one place 
#Adding the actual day number to the data
#First we create a hardcoded function that maps the waves to the dates, 
#then we're going to copy over the wave column into the new date column,
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

#Want to define all the reassigning to be an option instead of a mandatory reassignment
#this just covers all the "other" categories
#need to assign the output to something
#Call the function with full_data<-reassign_all_data(full_data)
reassign_all_data<-function(full_data){
#Need to recode values of "NA" in male or female race to be 6
full_data[is.na(full_data[["race"]]),][["race"]]<-6

#First we need to recode the values that have an "NA" in their field_cd to have a field_cd of 
#18 which corresponds to the Other field
full_data[is.na(full_data$field_cd),][["field_cd"]]<-18

#Need to recode NA values in some of the importance rankings and also in 
#the goals [of the speed dating event], how often they go out, and how often they go on dates
#Relevant columns:
#imprace, imprelig, goal, date, go_out
full_data[is.na(full_data[["imprace"]]),][["imprace"]]<-0
full_data[is.na(full_data[["imprelig"]]),][["imprelig"]]<-0
full_data[is.na(full_data[["goal"]]),][["goal"]]<-6
full_data[is.na(full_data[["date"]]),][["date"]]<-7
full_data[is.na(full_data[["go_out"]]),][["go_out"]]<-7

#How did the participant view the length of the event? 
#This is a scale of 1 to 3 where 1 is too little, 2 is too much, 3 is just right
#The way it's set up is kind of stupid if you ask me
full_data[is.na(full_data[["length"]]),][["length"]]<-3

#How did you think the number of speed dates that you had was?
#Scale of 1 to 3 where 1 is too little, 2 is too much, 3 is just right
full_data[is.na(full_data[["numdat_3"]]),][["numdat_3"]]<-3

full_data
} #End of the reassign_all_data function

#Function to clean activity columns to change NA's to 0's and say that the person had no interest in the activities
#Relevant columns:
#sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, and yoga
clean_NA_data<-function(full_data){
full_data[is.na(full_data[["sports"]]),][["sports"]]<-0
full_data[is.na(full_data[["tvsports"]]),][["tvsports"]]<-0
full_data[is.na(full_data[["exercise"]]),][["exercise"]]<-0
full_data[is.na(full_data[["dining"]]),][["dining"]]<-0
full_data[is.na(full_data[["museums"]]),][["museums"]]<-0
full_data[is.na(full_data[["art"]]),][["art"]]<-0
full_data[is.na(full_data[["hiking"]]),][["hiking"]]<-0
full_data[is.na(full_data[["gaming"]]),][["gaming"]]<-0
full_data[is.na(full_data[["clubbing"]]),][["clubbing"]]<-0
full_data[is.na(full_data[["reading"]]),][["reading"]]<-0
full_data[is.na(full_data[["tv"]]),][["tv"]]<-0
full_data[is.na(full_data[["theater"]]),][["theater"]]<-0
full_data[is.na(full_data[["movies"]]),][["movies"]]<-0
full_data[is.na(full_data[["concerts"]]),][["concerts"]]<-0
full_data[is.na(full_data[["music"]]),][["music"]]<-0
full_data[is.na(full_data[["shopping"]]),][["shopping"]]<-0
full_data[is.na(full_data[["yoga"]]),][["yoga"]]<-0

##Section of code to normalize the different rankings of categories that either a male or female prefers in a partner##
#For waves 1-5 and 10-21, the participant is asked to reassign weights that add up to 100 into 6 different categories
#while the participants in waves 6-9 are asked to give each attribute a rank from 1 to 10 where 1 implies that the 
#trait is not at all important while a rank of 10 implies that the trait is extremely important
#We also need to reassign the NA values to be 0's so that they don't skew the data when trying to perform functions on the fields
#Suspect that for waves 6-9 they have weighted their values by 100

#Clean all the first section at the same time
full_data[is.na(full_data[["attr1_1"]]),][["attr1_1"]]<-0
full_data[is.na(full_data[["sinc1_1"]]),][["sinc1_1"]]<-0
full_data[is.na(full_data[["intel1_1"]]),][["intel1_1"]]<-0
full_data[is.na(full_data[["fun1_1"]]),][["fun1_1"]]<-0
full_data[is.na(full_data[["amb1_1"]]),][["amb1_1"]]<-0
full_data[is.na(full_data[["shar1_1"]]),][["shar1_1"]]<-0

#Clean the attributes that a participant assigns to a partner on their scorecard to remove NAs
#relevant columns: attr, sinc, intel, fun, amb, shar
full_data[is.na(full_data[["attr"]]),][["attr"]]<-0
full_data[is.na(full_data[["sinc"]]),][["sinc"]]<-0
full_data[is.na(full_data[["intel"]]),][["intel"]]<-0
full_data[is.na(full_data[["fun"]]),][["fun"]]<-0
full_data[is.na(full_data[["amb"]]),][["amb"]]<-0
full_data[is.na(full_data[["shar"]]),][["shar"]]<-0

#Clean attributes that a participant writes halfway through the speed dating event
#that weights their views of importance of the attribute in a partner (similar to the 1_1 attributes)
#Relevant columns: attr1_s, sinc1_s, intel1_s, fun1_s,amb1_s, shar1_s
full_data[is.na(full_data[["attr1_s"]]),][["attr1_s"]]<-0
full_data[is.na(full_data[["sinc1_s"]]),][["sinc1_s"]]<-0
full_data[is.na(full_data[["intel1_s"]]),][["intel1_s"]]<-0
full_data[is.na(full_data[["fun1_s"]]),][["fun1_s"]]<-0
full_data[is.na(full_data[["amb1_s"]]),][["amb1_s"]]<-0
full_data[is.na(full_data[["shar1_s"]]),][["shar1_s"]]<-0

#Clean attributes that a participant writes halfway through the speed dating event
#that is the opinion of their own attributes
full_data[is.na(full_data[["attr3_s"]]),][["attr3_s"]]<-0
full_data[is.na(full_data[["sinc3_s"]]),][["sinc3_s"]]<-0
full_data[is.na(full_data[["intel3_s"]]),][["intel3_s"]]<-0
full_data[is.na(full_data[["fun3_s"]]),][["fun3_s"]]<-0
full_data[is.na(full_data[["amb3_s"]]),][["amb3_s"]]<-0
#full_data[is.na(full_data[["shar3_s"]]),][["shar3_s"]]<-0

#Clean attributes that a participant writes during the first followup after the speed dating event 
#that distributes weights onto the 6 attributes that they thought were important to their DECISIONS
#DURING THE EVENT
#Relevant columns: attr7_2, sinc7_2, intel7_2, fun7_2, amb7_2, shar7_2
full_data[is.na(full_data[["attr7_2"]]),][["attr7_2"]]<-0
full_data[is.na(full_data[["sinc7_2"]]),][["sinc7_2"]]<-0
full_data[is.na(full_data[["intel7_2"]]),][["intel7_2"]]<-0
full_data[is.na(full_data[["fun7_2"]]),][["fun7_2"]]<-0
full_data[is.na(full_data[["amb7_2"]]),][["amb7_2"]]<-0
full_data[is.na(full_data[["shar7_2"]]),][["shar7_2"]]<-0

#How satisfied the participant was with the people that they met during the event
full_data[is.na(full_data[["satis_2"]]),][["satis_2"]]<-0

#Participants were asked during the first followup survey to weight different attributes that
#they thought were IMPORTANT IN THE OPPOSITE SEX IN GENERAL
#Relevant columns: attr1_2, sinc1_2, intel1_2, fun1_2, amb1_2, shar1_2
full_data[is.na(full_data[["attr1_2"]]),][["attr1_2"]]<-0
full_data[is.na(full_data[["sinc1_2"]]),][["sinc1_2"]]<-0
full_data[is.na(full_data[["intel1_2"]]),][["intel1_2"]]<-0
full_data[is.na(full_data[["fun1_2"]]),][["fun1_2"]]<-0
full_data[is.na(full_data[["amb1_2"]]),][["amb1_2"]]<-0
full_data[is.na(full_data[["shar1_2"]]),][["shar1_2"]]<-0

#Participants were asked during the first followup survey to weight what most of their
#fellow men/women look for in the opposite sex/potential date
#Relevant columns: attr4_2, sinc4_2, intel4_2, fun4_2, amb4_2,shar4_2
full_data[is.na(full_data[["attr4_2"]]),][["attr4_2"]]<-0
full_data[is.na(full_data[["sinc4_2"]]),][["sinc4_2"]]<-0
full_data[is.na(full_data[["intel4_2"]]),][["intel4_2"]]<-0
full_data[is.na(full_data[["fun4_2"]]),][["fun4_2"]]<-0
full_data[is.na(full_data[["amb4_2"]]),][["amb4_2"]]<-0
full_data[is.na(full_data[["shar4_2"]]),][["shar4_2"]]<-0

#Participants were asked during the first followup survey to weigh what they think that 
#members of the opposite sex looks for in a potential date/partner
#Relevant columns: attr2_2, sinc2_2, intel2_2, fun2_2, amb2_2, shar2_2
full_data[is.na(full_data[["attr2_2"]]),][["attr2_2"]]<-0
full_data[is.na(full_data[["sinc2_2"]]),][["sinc2_2"]]<-0
full_data[is.na(full_data[["intel2_2"]]),][["intel2_2"]]<-0
full_data[is.na(full_data[["fun2_2"]]),][["fun2_2"]]<-0
full_data[is.na(full_data[["amb2_2"]]),][["amb2_2"]]<-0
full_data[is.na(full_data[["shar2_2"]]),][["shar2_2"]]<-0

#Participants were asked how they think that they measure up during the first followup survey
#Relevant columns: attr3_2, sinc3_2, intel3_2, fun3_2, amb3_2, shar3_2
full_data[is.na(full_data[["attr3_2"]]),][["attr3_2"]]<-0
full_data[is.na(full_data[["sinc3_2"]]),][["sinc3_2"]]<-0
full_data[is.na(full_data[["intel3_2"]]),][["intel3_2"]]<-0
full_data[is.na(full_data[["fun3_2"]]),][["fun3_2"]]<-0
full_data[is.na(full_data[["amb3_2"]]),][["amb3_2"]]<-0
#For some reason this is not recorded
#full_data[is.na(full_data[["shar3_2"]]),][["shar3_2"]]<-0

#Participants were asked how they think that others perceived 
#them during the first followup survey
#Relevant columns: attr5_2, sinc5_2, intel5_2, fun5_2, amb5_2, shar5_2
full_data[is.na(full_data[["attr5_2"]]),][["attr5_2"]]<-0
full_data[is.na(full_data[["sinc5_2"]]),][["sinc5_2"]]<-0
full_data[is.na(full_data[["intel5_2"]]),][["intel5_2"]]<-0
full_data[is.na(full_data[["fun5_2"]]),][["fun5_2"]]<-0
full_data[is.na(full_data[["amb5_2"]]),][["amb5_2"]]<-0
#For some reason this is not recorded
#full_data[is.na(full_data[["shar5_2"]]),][["shar5_2"]]<-0

#Participants were asked how many of their matches that they called, how many have called them, and how many dates they've been on
#Relevant columns: you_call, them_call, date_3, numdat_3, num_in_3
full_data[is.na(full_data[["you_call"]]),][["you_call"]]<-0
full_data[is.na(full_data[["them_cal"]]),][["them_cal"]]<-0
full_data[is.na(full_data[["date_3"]]),][["date_3"]]<-0
full_data[is.na(full_data[["num_in_3"]]),][["num_in_3"]]<-0

#Participants were asked during a second followup survey to rank the importance of certain attributes in
#the opposite sex
#Relevant columns: attr1_3, sinc1_3, intel1_3, fun1_3, amb1_3, shar1_3
full_data[is.na(full_data[["attr1_3"]]),][["attr1_3"]]<-0
full_data[is.na(full_data[["sinc1_3"]]),][["sinc1_3"]]<-0
full_data[is.na(full_data[["intel1_3"]]),][["intel1_3"]]<-0
full_data[is.na(full_data[["fun1_3"]]),][["fun1_3"]]<-0
full_data[is.na(full_data[["amb1_3"]]),][["amb1_3"]]<-0
full_data[is.na(full_data[["shar1_3"]]),][["shar1_3"]]<-0

#During the second follow up survey, they were asked to think back on the traits that
#they thought were the most important but we need to clean these values as well for NA's
#Relevant columns: attr7_3, sinc7_3, intel7_3, fun7_3, amb7_3,shar7_3
full_data[is.na(full_data[["attr7_3"]]),][["attr7_3"]]<-0
full_data[is.na(full_data[["sinc7_3"]]),][["sinc7_3"]]<-0
full_data[is.na(full_data[["intel7_3"]]),][["intel7_3"]]<-0
full_data[is.na(full_data[["fun7_3"]]),][["fun7_3"]]<-0
full_data[is.na(full_data[["amb7_3"]]),][["amb7_3"]]<-0
full_data[is.na(full_data[["shar7_3"]]),][["shar7_3"]]<-0

#Participants were asked in the second follow up survey what they think that other men/women 
#look for int he opposite sex
#Relevant columns: attr4_3, sinc4_3, intel4_3, fun4_3, amb4_3, shar4_3
full_data[is.na(full_data[["attr4_3"]]),][["attr4_3"]]<-0
full_data[is.na(full_data[["sinc4_3"]]),][["sinc4_3"]]<-0
full_data[is.na(full_data[["intel4_3"]]),][["intel4_3"]]<-0
full_data[is.na(full_data[["fun4_3"]]),][["fun4_3"]]<-0
full_data[is.na(full_data[["amb4_3"]]),][["amb4_3"]]<-0
full_data[is.na(full_data[["shar4_3"]]),][["shar4_3"]]<-0

#Participants were asked in the second followup survey what they think that members of the
#opposite sex look for in a partner
#Relevant columns: attr2_3, sinc2_3, intel2_3, fun2_3, amb2_3, shar2_3
full_data[is.na(full_data[["attr2_3"]]),][["attr2_3"]]<-0
full_data[is.na(full_data[["sinc2_3"]]),][["sinc2_3"]]<-0
full_data[is.na(full_data[["intel2_3"]]),][["intel2_3"]]<-0
full_data[is.na(full_data[["fun2_3"]]),][["fun2_3"]]<-0
full_data[is.na(full_data[["amb2_3"]]),][["amb2_3"]]<-0
full_data[is.na(full_data[["shar2_3"]]),][["shar2_3"]]<-0

#Participants were asked in the second followup survey to rate themselves on the 
#same attributes by weights that add to 100
#Relevant columns: attr3_3, sinc3_3, intel3_3, fun3_3, amb3_3, shar3_3
full_data[is.na(full_data[["attr3_3"]]),][["attr3_3"]]<-0
full_data[is.na(full_data[["sinc3_3"]]),][["sinc3_3"]]<-0
full_data[is.na(full_data[["intel3_3"]]),][["intel3_3"]]<-0
full_data[is.na(full_data[["fun3_3"]]),][["fun3_3"]]<-0
full_data[is.na(full_data[["amb3_3"]]),][["amb3_3"]]<-0
#This is not recorded for some reason
#full_data[is.na(full_data[["shar3_3"]]),][["shar3_3"]]<-0

#Participants were asked in the second followup survey to weight how they think
#that others perceived them
#Relevant columns: attr5_3, sinc5_3, intel5_3, fun5_3, amb5_3, shar5_3
full_data[is.na(full_data[["attr5_3"]]),][["attr5_3"]]<-0
full_data[is.na(full_data[["sinc5_3"]]),][["sinc5_3"]]<-0
full_data[is.na(full_data[["intel5_3"]]),][["intel5_3"]]<-0
full_data[is.na(full_data[["fun5_3"]]),][["fun5_3"]]<-0
full_data[is.na(full_data[["amb5_3"]]),][["amb5_3"]]<-0
#For some reason this is not recorded
#full_data[is.na(full_data[["shar5_3"]]),][["shar5_3"]]<-0

full_data
} #End of the clean_NA_data function

#Bootstrap function that creates training data and test data from the data, given a wave and gender
#Note that we can subset data as so: full_data[full_data$wave==1&full_data$gender==0,][1,]
#This will return the first row of the females in wave 1.
#For each individual in the wave-gender pair they see all of the people in the opposite gender in that wave 
#so in general for each wave-gender pair, there are num_males*num_females number of rows.
#However, we have on exception, in Wave 5 where there's a note that they're all undergrads, we have different
#numbers of rows between the wave 5 females and males.  Thus we predict that one of the females did not 
#have her data recorded for the 10 males.
# bootstrap_wave_gender<-function(w,g,type=c("training","test"),indices_list){
	# #Determine the number of people in this wave_gender pair
	# #Need to get the list of training or test vertices from the indices_list
	# #Need to define the name that we use to pull from the list
	# name<-paste(w,paste(g,type,sep="_"),sep="_")
	# indices_sample<-indices_list[[name]]
	
	# #For the best bootstrapping technique, we need to resample the data the same number of times as the number we have
	# #First we need to divide the data into training and test
	# range_total<-1:length(indices_sample)
	# sampling<-sample(range_total, length(indices_sample), replace=TRUE,prob=NULL)
	
	# return_data<-full_data[full_data$wave==w & full_data$gender==g,][indices_sample[sampling]]
	
	# return (return_data)
# }

#Test function
# wave_1_gender_0_train<-bootstrap_wave_gender(1,0,"training","age")
# wave_1_gender_0_test<-bootstrap_wave_gender(1,0,"test","age")

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

#We want to create a function that takes in a column name and returns the response rate
#thus we can see how reliable a column name is, on the fly
#type is a string denoting the gender or if it's not included then it'll default to the
#overall response rate
check_col_response_rate<-function(column_name,type){
	#Count the number of NA's first with the specifed other parameter which is gender
	if(type=="female"){
		how_many_na<-sum(is.na(full_data[full_data$gender==0,column_name]))
	}
	else if(type=="male"){
		how_many_na<-sum(is.na(full_data[full_data$gender==1,column_name]))
	}
	else{
	how_many_na<-sum(is.na(full_data[,column_name]))
	}
	
	#Define the response rate
	response_rate<-1-(how_many_na/length(full_data[,column_name]))
	
	return (response_rate)
}
