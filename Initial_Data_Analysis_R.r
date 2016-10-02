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


#To ensure a "match" where two people like each other, each person has to fill out a "yes" for the ID of the other person
#on their scorecard within that wave.
#i.e. for persons 1 and 2 to like each other, person 1 has to circle "yes" for person 2 and person 2 has to circle "yes" for person 1
#The person filling it out has their id within the wave in column "id" while their partner is in "pid"


