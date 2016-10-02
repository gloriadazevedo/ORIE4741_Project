#Initial data scanning notes

import csv


#Importing data
filename=open('C:\\Users\\glori\\Documents\\GitHub\\ORIE4741_Project\\Speed Dating Data.csv','r')
data=filename.read().splitlines()

#Close file
filename.close()

#Header row
header=data[0].split(',')

#Move all data into a matrix format
matrix=[]

for i in range(1,len(data)):
	matrix.append(data[i].split(','))


#Want to look at ranges of predictors and figure out which ones are important

