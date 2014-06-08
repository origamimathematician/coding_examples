#############PCA Project#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 2/21/2013
#
#An introduction to preforming MDS on
#multiply data sets.
#
#####################################

#Working directory for my PC
setwd("C:\\Users\\Pip\\Documents\\CS448\\MDSProject")

##################Iris Data####################

#takes the column containing the species associated with each
#data point, used later to colorize the plot
irisSpecies = iris$Species
#data points
irisData = iris[,1:4]

#generates a distance matrix of the data
featureData = dist(irisData)
#runs MDS on the distance matrix
featureData = cmdscale(featureData)

#set vectors for plots
xIris = featureData[,1]
yIris = featureData[,2]

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
#plots the data
plot(xIris,yIris, xlab = "Principle Coordinate 1", ylab = "Principle Coordinate 2",
	main = "MDS Plot for Iris Data Set", pch = 20, col = colorVector)
legend("topright", levels(irisSpecies), col = c("red","green","blue"),pch = 20)

#################Fruit Data####################
#reads in the fruit data as a data.frame
fruitInput = read.csv("fruit.csv")
#pulls the data points out of the fruit data
fruitData = fruitInput[,1:4]

#generates a distance matrix of the data
fruitFeatures = dist(fruitData)
#runs MDS on the distance matrix
fruitFeatures = cmdscale(fruitFeatures)

#sets vectors for the plots
xFruit = fruitFeatures[,1]
yFruit = fruitFeatures[,2]

#sets up vector for the colorization of the data points
colorVectorFruit = as.vector(fruitInput$Class)
colorVectorFruit[which(fruitInput$Class == "apple")] = "red"
colorVectorFruit[which(fruitInput$Class == "orange")] = "orange"
colorVectorFruit[which(fruitInput$Class == "lemon")] = "yellow"
colorVectorFruit[which(fruitInput$Class == "peach")] = "purple"

#################Fruit Data Plots####################
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the fruit data
plot(xFruit,yFruit,xlab="Principle Coordinate 1",ylab="Principle Coordinate 2",
	main="MDS Plot for Fruit Data Set",pch=20,col=colorVectorFruit)
legend("topleft", levels(fruitInput$Class), col=c("red","orange","yellow","purple"),
		pch=20)
		
##########################Mouse Data#########################
#reads in the mouse data from text file
mouseData = read.delim("MouseData.txt", row.names=1)
#transposes the data frame so that the row now represent samples from a mouse
mouseTranspose = t(mouseData)
#pulls the type of experiment from each data point, contains
#codes that tell what type of mouse was sampled and whether or not
#it was proximal or distal sample
experimentIDs = colnames(mouseData)
#creates a vector of the indicies corresponding to samples from the proxial end of the
# mouse colon
proxIndex = grep("[A-Z]+P[0-9]", experimentIDs)
#creates a vector of the indicies corresponding to samples from only B type mice
## not used BIndex = grep("B+[D,P][0-9]", experimentIDs)
#creates a vector of the indicies corresponding to samples from both B and C type mice
## not used BCIndex = grep("B+C+[D,P][0-9]", experimentIDs)
#creates a vector of the indicies corresponding to samples from only C type mice
CIndex = grep("^C[A-Z][0-9]$", experimentIDs)
#generates a distance matrix of the data
mouseFeatures = dist(mouseTranspose)
#runs MDS on the distance matrix
mouseFeatures = cmdscale(mouseFeatures)

#sets up vector for the colorization of the data points, sorts by
#whether sample was proximal or distal
colorVectorMouse = as.vector(experimentIDs)
colorVectorMouse[proxIndex] = "blue"
colorVectorMouse[-proxIndex] = "green"

#sorts by mouse type
colorVectorMouse2 = as.vector(experimentIDs)
colorVectorMouse2[CIndex] = "purple"
colorVectorMouse2[-CIndex] = "orange"

#sets different shaped data points for proximal and distal samples
shapeVectorMouse = as.vector(experimentIDs)
shapeVectorMouse[proxIndex] = 17
shapeVectorMouse[-proxIndex] = 20
shapeVectorMouse = as.numeric(shapeVectorMouse)

#sets vectors for plots
xMouse = mouseFeatures[,1]
yMouse = mouseFeatures[,2]

#################Mouse Data Plots####################
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the mouse data, colors by sample location
plot(xMouse,yMouse,xlab="Principle Coordinate 1",ylab="Principle Coordinate 2",
	main="MDS Plot for Mouse Data Set",pch=20,col=colorVectorMouse)
legend("topright", c("Proximal","Distal"), col=c("blue","green"),
		pch=20)
		
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the mouse data, colors by mouse type
plot(xMouse,yMouse,xlab="Principle Coordinate 1",ylab="Principle Coordinate 2",
	main="MDS Plot for Mouse Data Set",pch=20,col=colorVectorMouse2)
legend("topright", c("B Type Mouse", "C Type Mouse"),
		col=c("orange","purple"), pch=20)
		
windows(5,5)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the mouse data, colors by mouse type and distinguishes sample location by shape
plot(xMouse,yMouse,xlab="Principle Coordinate 1",ylab="Principle Coordinate 2",
	main="MDS Plot for Mouse Data Set",pch=shapeVectorMouse,col=colorVectorMouse2)
legend("topright", c("B Type Mouse", "C Type Mouse","Proximal","Distal"),
		col=c("orange","purple","black","black"), pch=c(20,20,20,17,20))
		
##########################Tumor Data#########################
#reads in tumor data from data file
tumorData = read.csv("tumorData.data")
#pulls tumor classes (benign or malignant) from data
tumorClasses = tumorData[,11]
#pulls tumor measurements from data
tumorData = tumorData[,2:10]
#generates a distance matrix of the data
tumorFeatures = dist(tumorData)
#runs MDS on the distance matrix
tumorFeatures = cmdscale(tumorFeatures)

#sets vectors for plots
xTumor = tumorFeatures[,1]
yTumor = tumorFeatures[,2]

#creates a color vector for the data points, colorizes by class
colorVectorTumor = as.vector(tumorClasses)
colorVectorTumor[which(tumorClasses == 2)] = "blue"
colorVectorTumor[which(tumorClasses == 4)] = "red"

#################Tumor Data Plots####################
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the mouse data, colors by sample location
plot(xTumor,yTumor,xlab="Principle Coordinate 1",ylab="Principle Coordinate 2",
	main="MDS Plot for Tumor Data Set",pch=20,col=colorVectorTumor)
legend("topright", c("Benign","Malignant"), col=c("blue","red"),
		pch=20)