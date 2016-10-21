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

#Need to iterate through each wave to get the id's of males and females
scrape_data<-function(column_vector){
	result_vector<-rep(0,total_interactions)
	return_matrix<-matrix(rep(0,2*length(column_vector)*total_interactions),ncol=2*length(column_vector),nrow=total_interactions)

	counter<-1
	for (w in 1:num_waves){
		#Figure out the ids in each wave
		female_id<-unique(full_data[full_data$wave==w & full_data$gender==0,]$id)
		male_id<-unique(full_data[full_data$wave==w & full_data$gender==1,]$id)
		
		#Data extraction by match
		for (i in 1:length(female_id)){
			#Need to just extract the relevant columns and rows for the female
			f_vector<-full_data[full_data$wave==w & full_data$gender==0 & 
			full_data$id==female_id[i],column_vector][1,]
			
			for (j in 1:length(male_id)){
				#Need to extract the relevant columns and rows for the male
				m_vector<-full_data[full_data$wave==w & full_data$gender==1 & 
					full_data$id==male_id[j],column_vector][1,]
				
				#Obtain the match value for the vector
				match_value<-full_data[full_data$wave==w & full_data$gender==0 & 
				full_data$id==female_id[i] & full_data$partner==male_id[j], ]$"match"
				
				#Update the resulting classification vector
				result_vector[counter]<-match_value
				return_matrix[counter,1:length(column_vector)]<-t(f_vector)
				return_matrix[counter,(length(column_vector)+1):(2*length(column_vector))]<-t(m_vector)
				counter<-counter+1
			}
		}
	}
	return_list<-list(return_matrix=return_matrix,result_vector=result_vector)
	return(return_list)
}

#Call the function to test
column_vector<-c("race","field_cd","yoga")
result_list<-scrape_data(column_vector)
return_matrix<-result_list$return_matrix
result_vector<-result_list$result_vector

#Run a linear model to test
test_lm<-lm(result_vector~.,data=data.frame(return_matrix))
summary(test_lm)

#Some other issues that we've encountered when looking at the data are that,
#even though we're using coded values for race and field_cd, it's more important
#that the races and field_cds are the same for the two individuals, not the
#difference in magnitude, since they are still categorical variables.
#For race there's already a column called "samerace" but there isn't one for field_cd
#Also then if we wanted to pull those columns, we would get them twice (in two columns)
#in this current setup.