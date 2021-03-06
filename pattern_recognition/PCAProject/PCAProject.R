#############PCA Project#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 2/06/2013
#
#An introduction to preforming PCA on
#some simple data sets.
#
#####################################

#set working directoy
setwd("C:\\Users\\Pip\\Documents\\CS448\\PCAProject")
library(rgl)

#################Iris Data####################
#takes the column containing the species associated with each
#data point, used later to colorize the plot
irisSpecies = iris$Species
#data points
irisData = iris[,1:4]
#runs PCA on the data points
featureData = prcomp(irisData)
#projects the original data points on to the first and second
#principle components, sets them to variables to be plotted later
x = featureData$x[,1]
y = featureData$x[,2]
#instantiates the color vector as the so that it initial holds
#the species names
colorVector = as.vector(irisSpecies)
#sets a distinct color for each species
colorVector[which(irisSpecies == "setosa")] = "red"
colorVector[which(irisSpecies == "versicolor")] = "green"
colorVector[which(irisSpecies == "virginica")] = "blue"

#################Iris Data Plots####################
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the projected data against the first and second
#principle components
plot(x,y, xlab = "Principal Component 1", ylab = "Principal Component 2",
	main = "PCA Plot for Iris Data Set", pch = 20, col = colorVector)
legend("topright", levels(irisSpecies), col = c("red","green","blue"),pch = 20)
#calculates the stadard deviation for the principle components
sdevVector = featureData$sdev
#converts standard deviations into proportions of total variance
varianceDat = (sdevVector*sdevVector/sum(sdevVector*sdevVector))
#sets up data for bar plot
xBar = colnames(featureData$rotation)
yBar = varianceDat
windows(4,4)
#bar plot of the variance proportions
barplot(yBar, width = 1, names.arg = xBar, main = "Porportions of Variance (Iris)")

#################Fruit Data####################
#reads in the fruit data as a data.frame
fruitInput = read.csv("fruit.csv")
#pulls the data points out of the fruit data
fruitData = fruitInput[,1:4]
#PCA on the fruit data
featureData2 = prcomp(fruitData)
#data projection onto the first and second principle components
x2 = featureData2$x[,1]
y2 = featureData2$x[,2]
#sets up vector for the colorization of the data points
colorVectorFruit = as.vector(fruitInput$Class)
colorVectorFruit[which(fruitInput$Class == "apple")] = "red"
colorVectorFruit[which(fruitInput$Class == "orange")] = "orange"
colorVectorFruit[which(fruitInput$Class == "lemon")] = "yellow"
colorVectorFruit[which(fruitInput$Class == "peach")] = "burlywood1"

#################Fruit Data Plots####################
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#first plot of the fruit data against the first and second principle
#components
plot(x2,y2,xlab="Principle Component 1",ylab="Principle Component 2",
	main="PCA Plot for Fruit Data Set",pch=20,col=colorVectorFruit)
legend("topleft", levels(fruitInput$Class), col=c("red","orange","yellow","burlywood1"),
		pch=20)
#standard deviation and variance proportion calculations
sdevVectorFruit = featureData2$sdev
varianceDatFruit = (sdevVectorFruit*sdevVectorFruit/sum(sdevVectorFruit*sdevVectorFruit))
#sets up data for bar plot
xBar2 = colnames(featureData2$rotation)
yBar2 = varianceDatFruit
windows(4,4)
#bar plot of the proportions of variance
barplot(yBar2,width=1,names.arg = xBar2,main = "Porportions of Variance (Fruit)")

########################3D Fruit Data Plot###########################
#projects data points against the third principle component
z=featureData2$x[,3]
#creates a 3D plot of the fruit data
plot3d(x2,y2,z,xlab="Principle Component 1",ylab="Principle Component 2",
		zlab="Principle Component 3",main="PCA Plot-Fruit Data 3D",pch=20,
		col=colorVectorFruit)
#####################################################################

#projects the fruit data against the second and third principle components
x3 = featureData2$x[,2]
y3 = featureData2$x[,3]
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#creates the second fruit data plot, this time useing the second and third
#principle components
plot(x3,y3,xlab="Principle Component 2",ylab="Principle Component 3",
	main="PCA Plot #2 for Fruit Data Set",pch=20,col=colorVectorFruit)
legend("topleft", levels(fruitInput$Class), col=c("red","orange","yellow","burlywood1"),
		pch=20)
########################## END ###################################
