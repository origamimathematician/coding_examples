#clears any existing variables
rm(list=ls())
#stores the built-in iris data set to the variable 'l1'
l1 = iris
#prints the iris data from the 'l1' variable
print(l1)
#lists all the variables currently in mem
ls()
#write iris data to a .csv file
write.csv(l1,"irisData.csv")
#reads in the iris data from irisData.csv
l2 = read.csv("irisData.csv")
#prints the last column of the iris data that contains 
#the species names
print(l2[,c('Species')])
#prints data type of the iris data
print(class(l2))
#prints the first row of the data frame
print(l2[1,])
#prints out each row of the iris data using the tapply command
apply(l2, 1, print)