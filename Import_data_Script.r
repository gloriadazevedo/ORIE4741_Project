#Import data after you change into the directory where the data is stored
setwd("/Users/glori/Documents/GitHub/ORIE4741_Project")

full_data<-read.table("data_excel.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
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


##################General statistics##################
#How many waves
num_waves<-max(full_data[!is.na(full_data$wave),]$wave)

#Frequency of each wave, total and by gender
wave_freq<-table(full_data$wave)
wave_gender_freq<-table(full_data$wave,full_data$gender)

#How many matches 
#Need to divide by 2 to not double count for the "female" view and the "male" view
total_match<-sum(full_data$"match")/2

#Want to find the total number of interactions between people
#Need to do this manually for some reason
check<-0
for (w in 1:num_waves){
	female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
	male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
	for (i in 1:length(female_id)){
		for (j in 1:length(male_id)){
			check<-check+1
		}
	}
}
total_interactions<-check

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

#Need to recode NA values in some of the importance rankings and also in 
#the goals [of the speed dating event], how often they go out, and how often they go on dates
#Relevant columns:
#imprace, imprelig, goal, date, go_out
for(i in 1:length(full_data$imprace)){
	if (is.na(full_data[i,]$imprace)){
		full_data[i,]$imprace<-0
	}
	if (is.na(full_data[i,]$imprelig)){
		full_data[i,]$imprelig<-0
	}
	if (is.na(full_data[i,]$goal)){
		full_data[i,]$goal<-6
	}
	if (is.na(full_data[i,]$"date")){
		full_data[i,]$"date"<-7
	}
	if (is.na(full_data[i,]$go_out)){
		full_data[i,]$go_out<-7
	}
}

#Consider adding a field denoting whether or not the school that they went to 
#for undergraduate is an Ivy League school or other classification
#First get a unique list of the schools that they went to for undergraduate
undergraduate<-unique(full_data$undergra)

#First change all NA values to "Other"
for (i in 1:length(full_data$undergra)){
	if (is.na(full_data$undergra[i])){
		full_data$undergra[i]<-"Other"
		}
}

#Then change all blanks to "Other"
for (i in 1:length(full_data$undergra)){
	if (full_data$undergra[i]==""){
		full_data$undergra[i]<-"Other"
		}
}

#Then we get a list of 242 universities although note that some of them are the same but typed in differently
#i.e. "GW" should mean "George Washington University"
#Still have a lot of blanks or "Other" so we don't think that this is a reliable predictor for compatibility

#Section to clean activity columns to change NA's to 0's and say that the person had no interest in the activities
#Relevant columns:
#sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, and yoga
for (i in 1:length(full_data$sports)){
	if(is.na(full_data[i,]$sports)){
		full_data[i,]$sports<-0
	}
	if(is.na(full_data[i,]$tvsports)){
		full_data[i,]$tvsports<-0
	}
	if(is.na(full_data[i,]$exercise)){
		full_data[i,]$exercise<-0
	}
	if(is.na(full_data[i,]$dining)){
		full_data[i,]$dining<-0
	}
	if(is.na(full_data[i,]$museums)){
		full_data[i,]$museums<-0
	}
	if(is.na(full_data[i,]$art)){
		full_data[i,]$art<-0
	}
	if(is.na(full_data[i,]$hiking)){
		full_data[i,]$hiking<-0
	}
	if(is.na(full_data[i,]$gaming)){
		full_data[i,]$gaming<-0
	}
	if(is.na(full_data[i,]$clubbing)){
		full_data[i,]$clubbing<-0
	}
	if(is.na(full_data[i,]$reading)){
		full_data[i,]$reading<-0
	}
	if(is.na(full_data[i,]$tv)){
		full_data[i,]$tv<-0
	}
	if(is.na(full_data[i,]$theater)){
		full_data[i,]$theater<-0
	}
	if(is.na(full_data[i,]$movies)){
		full_data[i,]$movies<-0
	}
	if(is.na(full_data[i,]$concerts)){
		full_data[i,]$concerts<-0
	}
	if(is.na(full_data[i,]$music)){
		full_data[i,]$music<-0
	}
	if(is.na(full_data[i,]$shopping)){
		full_data[i,]$shopping<-0
	}
	if(is.na(full_data[i,]$yoga)){
		full_data[i,]$yoga<-0
	}
}


##Section of code to normalize the different rankings of categories that either a male or female prefers in a partner##
#For waves 1-5 and 10-21, the participant is asked to reassign weights that add up to 100 into 6 different categories
#while the participants in waves 6-9 are asked to give each attribute a rank from 1 to 10 where 1 implies that the 
#trait is not at all important while a rank of 10 implies that the trait is extremely important
#We also need to reassign the NA values to be 0's so that they don't skew the data when trying to perform functions on the fields
#Suspect that for waves 6-9 they have weighted their values by 100

# attr1_1_temp_vector<-rep(0,length(full_data$attr1_1))
# sinc1_1_temp_vector<-rep(0,length(full_data$sinc1_1))
# intel1_1_temp_vector<-rep(0,length(full_data$intel1_1))
# fun1_1_temp_vector<-rep(0,length(full_data$fun1_1))
# amb1_1_temp_vector<-rep(0,length(full_data$amb1_1))
# shar1_1_temp_vector<-rep(0,length(full_data$shar1_1))

#Clean all the first section at the same time
for (i in 1:length(full_data$attr1_1)){
	#wave_num<-full_data[i,]$wave
	
	#Also need a check for NA values
	#Change the original one and also change it in the temporary vector
	if(is.na(full_data[i,]$attr1_1)){
		full_data[i,]$attr1_1<-0
		#attr1_1_temp_vector[i]<-0
	}
	if(is.na(full_data[i,]$sinc1_1)){
		full_data[i,]$sinc1_1<-0
		#sinc1_1_temp_vector[i]<-0
	}
	if(is.na(full_data[i,]$intel1_1)){
		full_data[i,]$intel1_1<-0
		#intel1_1_temp_vector[i]<-0
	}
	if(is.na(full_data[i,]$fun1_1)){
		full_data[i,]$fun1_1<-0
		#fun1_1_temp_vector[i]<-0
	}
	if(is.na(full_data[i,]$amb1_1)){
		full_data[i,]$amb1_1<-0
		#amb1_1_temp_vector[i]<-0
	}
	if(is.na(full_data[i,]$shar1_1)){
		full_data[i,]$shar1_1<-0
		#shar1_1_temp_vector[i]<-0
	}
	
	#Commenting out for now since they're all weighted the same
	#Check wave values
	# if(wave_num>=1 & wave_num<=5){
		# attr1_1_temp_vector[i]<-(full_data[i,]$attr1_1)/10
		# sinc1_1_temp_vector[i]<-(full_data[i,]$attr1_1)/10
	# }
	# if(wave_num>=6 & wave_num<=9){
		# attr1_1_temp_vector[i]<-(full_data[i,]$attr1_1)/10
	# }
	# if(wave_num>=10 & wave_num<=21){
		# attr1_1_temp_vector[i]<-(full_data[i,]$attr1_1)/10
	# }
}

#Clean the attributes that a participant assigns to a partner on their scorecard to remove NAs
#relevant columsn: attr, sinc, intel, fun, amb, shar
for (i in 1:length(full_data$"attr")){
	if(is.na(full_data[i,]$"attr")){
		full_data[i,]$"attr"<-0
	}
	if(is.na(full_data[i,]$sinc)){
		full_data[i,]$sinc<-0
	}
	if(is.na(full_data[i,]$intel)){
		full_data[i,]$intel<-0
	}
	if(is.na(full_data[i,]$fun)){
		full_data[i,]$fun<-0
	}
	if(is.na(full_data[i,]$amb)){
		full_data[i,]$amb<-0
	}
	if(is.na(full_data[i,]$shar)){
		full_data[i,]$shar<-0
	}
}

#Clean attributes that a participant writes halfway through the speed dating event
#that weights their views of importance of the attribute in a partner (similar to the 1_1 attributes)
#Relevant columns: attr1_s, sinc1_s, intel1_s, fun1_s,amb1_s, shar1_s

for (i in 1:length(full_data$attr1_s)){
	if (is.na(full_data[i,]$attr1_s)){
		full_data[i,]$attr1_s<-0
	}
	if(is.na(full_data[i,]$sinc1_s)){
		full_data[i,]$sinc1_s<-0
	}
	if(is.na(full_data[i,]$intel1_s)){
		full_data[i,]$intel1_s<-0
	}
	if(is.na(full_data[i,]$fun1_s)){
		full_data[i,]$fun1_s<-0
	}
	if(is.na(full_data[i,]$amb1_s)){
		full_data[i,]$amb1_s<-0
	}
	if(is.na(full_data[i,]$shar1_s)){
		full_data[i,]$shar1_s<-0
	}
}

#Clean attributes that a participant writes during the first followup after the speed dating event 
#that distributes weights onto the 6 attributes that they thought were important to their DECISIONS
#DURING THE EVENT
#Relevant columns: attr7_2, sinc7_2, intl7_2, fun7_2, amb7_2, shar7_2

for (i in 1:length(full_data$attr7_2)){
	if(is.na(full_data[i,]$attr7_2)){
		full_data[i,]$attr7_2<-0
	}
	if(is.na(full_data[i,]$sinc7_2)){
		full_data[i,]$sinc7_2<-0
	}
	if(is.na(full_data[i,]$intel7_2)){
		full_data[i,]$intel7_2<-0
	}
	if(is.na(full_data[i,]$fun7_2)){
		full_data[i,]$fun7_2<-0
	}
	if(is.na(full_data[i,]$amb7_2)){
		full_data[i,]$amb7_2<-0
	}
	if(is.na(full_data[i,]$shar7_2)){
		full_data[i,]$shar7_2<-0
	}
}

#Participants were asked during the first followup survey to weight different attributes that
#they thought were IMPORTANT IN THE OPPOSITE SEX IN GENERAL
#Relevant columns: attr 1_2, sinc1_2, intel1_2, fun1_2, amb1_2, shar1_2
for (i in 1:length(full_data$attr1_2)){
	if(is.na(full_data[i,]$attr1_2)){
		full_data[i,]$attr1_2<-0
	}
	if(is.na(full_data[i,]$sinc1_2)){
		full_data[i,]$sinc1_2<-0
	}
	if(is.na(full_data[i,]$intel1_2)){
		full_data[i,]$intel1_2<-0
	}
	if(is.na(full_data[i,]$fun1_2)){
		full_data[i,]$fun1_2<-0
	}
	if(is.na(full_data[i,]$amb1_2)){
		full_data[i,]$amb1_2<-0
	}
	if(is.na(full_data[i,]$shar1_2)){
		full_data[i,]$shar1_2<-0
	}
}

#Participants were asked during a second followup survey to rank the importance of certain attributes in
#the opposite sex
#Relevant columns: attr1_3, sinc1_3, intel1_3, fun1_3, amb1_3, shar1_3
for (i in 1:length(full_data$attr1_3)){
	if(is.na(full_data[i,]$attr1_3)){
		full_data[i,]$attr1_3<-0
	}
	if(is.na(full_data[i,]$sinc1_3)){
		full_data[i,]$sinc1_3<-0
	}
	if(is.na(full_data[i,]$intel1_3)){
		full_data[i,]$intel1_3<-0
	}
	if(is.na(full_data[i,]$fun1_3)){
		full_data[i,]$fun1_3<-0
	}
	if(is.na(full_data[i,]$amb1_3)){
		full_data[i,]$amb1_3<-0
	}
	if(is.na(full_data[i,]$shar1_3)){
		full_data[i,]$shar1_3<-0
	}
}

#During the second follow up survey, they were asked to think back on the traits that
#they thought were the most important but we need to clean these values as well for NA's
#Relevant columns: attr7_3, sinc7_3, intel7_3, fun7_3, amb7_3,shar7_3
for(i in 1:length(full_data$attr7_3)){
	if(is.na(full_data[i,]$attr7_3)){
		full_data[i,]$attr7_3<-0
	}
	if(is.na(full_data[i,]$sinc7_3)){
		full_data[i,]$sinc7_3<-0
	}
	if(is.na(full_data[i,]$intel7_3)){
		full_data[i,]$intel7_3<-0
	}
	if(is.na(full_data[i,]$fun7_3)){
		full_data[i,]$fun7_3<-0
	}
	if(is.na(full_data[i,]$amb7_3)){
		full_data[i,]$amb7_3<-0
	}
	if(is.na(full_data[i,]$shar7_3)){
		full_data[i,]$shar7_3<-0
	}
}


#Since we are considering using race as a predictor, we also want to know the breakdown
#of the number of people in race overall and by gender
total_race_vector<-rep(0,6)
female_race_vector<-rep(0,6)
male_race_vector<-rep(0,6)

#For each wave, we get the male and female ids
#then for each of those, get the first value in the race vector and then increment
for (w in 1:num_waves){
	#List of female and male ids
	female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
	male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
	
	#Run through all the female ids first. 
	#Can do the female and male ids in separate loops since we're not comparing their race
	#Also the number of females can be different than the number of males in a wave
	for(i in 1:length(female_id)){
		race<-full_data[full_data$wave==w & full_data$gender==0 & full_data$id==female_id[i],]$race[1]
		total_race_vector[race]<-total_race_vector[race]+1
		female_race_vector[race]<-female_race_vector[race]+1
	}
	
	for (i in 1:length(male_id)){
		race<-full_data[full_data$wave==w & full_data$gender==1 & full_data$id==male_id[i],]$race[1]
		total_race_vector[race]<-total_race_vector[race]+1
		male_race_vector[race]<-male_race_vector[race]+1
	}
}

#Look at outputs
#race=5 corresponds to Native American
#race=3 corresponds to Hispanic--also historically a minority in higher education, unfortunately
#Thankfully if we add the female_race_vector and the male_race_vector we get the total_race_vector
total_race_vector
#Output: [1]  26 304  42 136   0  37

female_race_vector
#Output: [1]  16 142  25  71   0  16

male_race_vector
#Output: [1]  10 162  17  65   0  21