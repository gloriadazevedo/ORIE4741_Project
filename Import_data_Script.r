#Import data after you change into the directory where the data is stored
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

#How many matches
total_match<-sum(full_data$"match")/2


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