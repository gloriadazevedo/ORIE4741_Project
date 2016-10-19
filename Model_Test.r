#This R file will be an exhaustive, well-documented file with models that we've tried 
#as well as the data processing before each model was made in case it's not explicit

#Want to find a model that's robust but also accurate and have sparse solutions
#so that in the future, we don't have to have a super long questionnaire for 
#new participants in the speed dating environment or in online dating sites.

#We should make sure that we include the same questions for both males and females in the 
#questionnaire, otherwise the asymmetry could cause bias in preferences.

#How do we scrape the full data set to get all the information about the pairs that we want?
#column_vector is a vector of the column names that we want for each of male and female
#The result_vector is always going to be returned as the match indicator.
#If we use all the data to develop the model then the length of it is the total number of 
#interactions, total_interactions. Later if we want to develop the model on the subset of the 
#match data to test it on the other disjoint half or use bootstrap or k-fold cross validation,
#then we will divide the total matrix up.

column_vector<-c(race,field_cd)

#Use an external counter for the subset data matrix and result vector to explicitly
#assign data to the corresponding rows instead of "append" 
result_vector<-rep(0,total_interactions)
return_matrix<-matrix(rep(0,length(column_vector)*total_interactions),ncol=length(column_vector),nrow=total_interactions)
counter<-1

#Need to iterate through each wave to get the id's of males and females
scrape_data<-function(column_vector){
	for (w in 1:num_waves){
		#Figure out the ids in each wave
		female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
		male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
		
		#Data extraction by match
		for (i in 1:length(female_id)){
			for (j in 1:length(male_id)){
				
				
				#Update the resulting classification vector
				result_vector[counter]<-match_value
				counter<-counter+1
			}
		}
	}
}