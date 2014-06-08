#############PCA Project#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 3/9/2013
#
#An introduction to preforming LDA on
#data sets.
#
#####################################

library(MASS)
setwd("C:\\Users\\Pip\\Documents\\CS448\\LDAProject")

###################Iris LDA##################
#reads in iris data
rawData = iris
#pulls out the numeric data from the iris set
numericData = rawData[1:4]
#class vector for iris data
irisClasses = iris$Species
#randomly picks 125 samples from the iris data as the training set
#for the LDA classifier
trainingIndex = sample(1:150,125)
#runs LDA on the training set
irisLDA = lda(numericData[trainingIndex,], irisClasses[trainingIndex])
#attempts to classify the remaing 25 samples based on the results from
#the LDA analysis
classify = predict(irisLDA, numericData[-trainingIndex, ])$class
#number of incorrectly identified samples
accuracy = classify[which(classify != irisClasses[-trainingIndex])]
#prints a statement about overall classification accuracy
paste("Classification accuracy was ", round(100*(25-length(accuracy))/25, 2), "%.", sep ="")

xLDA = as.matrix(numericData) %*% irisLDA$scaling[,1]
yLDA = -1*as.matrix(numericData) %*% irisLDA$scaling[,2]

#####################Iris MDS########################

#generates a distance matrix of the data
featureData = dist(numericData)
#runs MDS on the distance matrix
featureData = cmdscale(featureData)

#set vectors for plots
xMDS = featureData[,1]
yMDS = featureData[,2]

#####################Iris PCA########################

#runs PCA on the data points
featureData = prcomp(numericData)
#projects the original data points on to the first and second
#principle components, sets them to variables to be plotted later
xPCA = featureData$x[,1]
yPCA = featureData$x[,2]

#################Iris Comparison Plots###################
#instantiates the color vector as the so that it initial holds
#the species names
colorVector = as.vector(irisClasses)
#sets a distinct color for each species
colorVector[which(irisClasses == "setosa")] = "red"
colorVector[which(irisClasses == "versicolor")] = "green"
colorVector[which(irisClasses == "virginica")] = "blue"

#sets up a plotting window with three subplots, one for each method of analysis
windows(6.6,2.2)
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)
par(mar = c(2.5,2.5,1.,1), mgp = c(1.5,.5,0))

#plots the data projected onto the first and second linear discriminates
plot(xLDA, yLDA, xlab = "Linear Discriminate 1", ylab = "Linear Discriminate 2", pch = 20, col = colorVector)
mtext("(A)", line = -2)

#plots the data for the first and second principle coordinates
plot(xMDS,yMDS, xlab = "Principle Coordinate 1", ylab = "Principle Coordinate 2", pch = 20, col = colorVector)
mtext("(B)", line = -2)

#plots the projected data against the first and second
#principle components
plot(xPCA,yPCA, xlab = "Principal Component 1", ylab = "Principal Component 2", pch = 20, col = colorVector)
mtext("(C)", line = -2)

#####################Fruit Data#####################

#reads in the fruit data as a data.frame
fruitInput = read.csv("fruit.csv")
#pulls the data points out of the fruit data
fruitData = fruitInput[,1:4]
fruitTypes = fruitInput[,5]

#************** PCA ****************#
#runs PCA on fruit data
fruitFeaturesPCA = prcomp(fruitData)
#data projection onto the first and second principle components
xFruitPCA = fruitFeaturesPCA$x[,1]
yFruitPCA = fruitFeaturesPCA$x[,2]

#************** MDS ****************#
#generates a distance matrix of the data
fruitFeaturesMDS = dist(fruitData)
#runs MDS on the distance matrix
fruitFeaturesMDS = cmdscale(fruitFeaturesMDS)

#sets vectors for the plots
xFruitMDS = fruitFeaturesMDS[,1]
yFruitMDS = fruitFeaturesMDS[,2]

#***************Interesting MDS and PCA Comparison**************#
#reflects the data on the first principle coordinate so that it matches the
#order of the first principle coordinate
xMDSCompare = -1*xFruitMDS
xPCACompare = xFruitPCA
#calculates the mean differences between the values for the data projected onto 
# the first principle component and the data on the first principle coordinate
meanDiff = mean(xMDSCompare[1:1000] - xPCACompare[1:1000])
#prints out mean difference
paste("The mean difference between the first principle coordinate and the first principle component values was ",
 round(meanDiff,20), ".", sep = "")

#************** LDA ****************#
#random selection of 800 samples from the fruit data( out of 1000 total)
trainingIndexF = sample(1:1000,800)
#runs LDA on fruit data
fruitLDA = lda(fruitData[trainingIndexF,], fruitTypes[trainingIndexF])
#attempts to classify the remaining 200 samples based on the results from
#the LDA analysis
classifyF = predict(fruitLDA, fruitData[-trainingIndexF, ])$class
#number of incorrectly identified samples
accuracyF = classifyF[which(classifyF != fruitTypes[-trainingIndexF])]
#prints a statement about overall classification accuracy
paste("Classification accuracy was ", 
round(100*(length(fruitTypes)-length(accuracyF))/length(fruitTypes), 2), "%.", sep ="")

#Projects data onto first and second linear discriminates
xFruitLDA = as.matrix(fruitData) %*% fruitLDA$scaling[,1]
yFruitLDA = as.matrix(fruitData) %*% fruitLDA$scaling[,2]

#################Fruit Comparison Plots###################

#sets up vector for the colorization of the data points
colorVectorFruit = as.vector(fruitInput$Class)
colorVectorFruit[which(fruitInput$Class == "apple")] = "red"
colorVectorFruit[which(fruitInput$Class == "orange")] = "orange"
colorVectorFruit[which(fruitInput$Class == "lemon")] = "yellow"
colorVectorFruit[which(fruitInput$Class == "peach")] = "purple"

#sets up a plotting window with three subplots, one for each method of analysis
windows(6.6,2.2)
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)
par(mar = c(2.5,2.5,1.,1), mgp = c(1.5,.5,0))

#plots the data projected onto the first and second linear discriminates
plot(xFruitLDA, yFruitLDA, xlab = "Linear Discriminate 1", ylab = "Linear Discriminate 2", pch = 20, col = colorVectorFruit)
mtext("(A)", line = -2)

#plots the data for the first and second principle coordinates
plot(xFruitMDS,yFruitMDS, xlab = "Principle Coordinate 1", ylab = "Principle Coordinate 2", pch = 20, col = colorVectorFruit)
mtext("(B)", line = -2)

#plots the projected data against the first and second
#principle components
plot(xFruitPCA,yFruitPCA, xlab = "Principal Component 1", ylab = "Principal Component 2", pch = 20, col = colorVectorFruit)
mtext("(C)", line = -2)

#####################Mouse Data#####################

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
# sets mouse classes based on sample location
mouseClasses = experimentIDs
mouseClasses[proxIndex] = "proximal"
mouseClasses[-proxIndex] = "distal"

#**********************LDA************************#

#runs LDA on mouse data
mouseLDA = lda(mouseTranspose, mouseClasses, tol=0)
#classifies mouse data (Note: training set == test set)
classifyM = predict(mouseLDA, mouseTranspose)$class
#number of incorrectly identified samples
accuracyM = classifyM[which(classifyM != mouseClasses)]
#prints a statement about overall classification accuracy (NOT reliable as is!)
paste("Classification accuracy was ", 
round(100*(length(mouseClasses)-length(accuracyM))/length(mouseClasses), 2), "%.", sep ="")

#Projects data onto first and second linear discriminates
xMouseLDA = as.matrix(mouseTranspose) %*% mouseLDA$scaling[,1]
yMouseLDA = as.matrix(mouseTranspose) %*% mouseLDA$scaling[,2]

#**********************MDS************************#

#generates a distance matrix of the data
mouseFeaturesMDS = dist(mouseTranspose)
#runs MDS on the distance matrix
mouseFeaturesMDS = cmdscale(mouseFeaturesMDS)

#sets vectors for plots
xMouseMDS = mouseFeaturesMDS[,1]
yMouseMDS = mouseFeaturesMDS[,2]

#**********************PCA************************#

#runs PCA on mouse data
mouseFeaturesPCA = prcomp(mouseTranspose)

#projects data onto principle components
xMousePCA = mouseFeaturesPCA$x[,1]
yMousePCA = mouseFeaturesPCA$x[,2]


#################Mouse Comparison Plots###################
#sets up vector for the colorization of the data points, sorts by
#whether sample was proximal or distal
colorVectorMouse = as.vector(experimentIDs)
colorVectorMouse[proxIndex] = "blue"
colorVectorMouse[-proxIndex] = "green"

#sets up a plotting window with three subplots, one for each method of analysis
windows(6.6,2.2)
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)
par(mar = c(2.5,2.5,1.,1), mgp = c(1.5,.5,0))

#plots the data projected onto the first and second linear discriminates
plot(xMouseLDA, yMouseLDA, xlab = "Linear Discriminate 1", ylab = "Linear Discriminate 2", pch = 20, col = colorVectorMouse)
mtext("(A)", line = -2)

#plots the data for the first and second principle coordinates
plot(xMouseMDS,yMouseMDS, xlab = "Principle Coordinate 1", ylab = "Principle Coordinate 2", pch = 20, col = colorVectorMouse)
mtext("(B)", line = -2)

#plots the projected data against the first and second
#principle components
plot(xMousePCA,yMousePCA, xlab = "Principal Component 1", ylab = "Principal Component 2", pch = 20, col = colorVectorMouse)
mtext("(C)", line = -2)

#####################Tumor Data#####################

#reads in tumor data from data file, places an NA string for missing metrics
tumorData = read.csv("tumorData.data", na.string = "?")
#pulls tumor classes (benign or malignant) from data
tumorClasses = tumorData[,11]
#pulls tumor measurements from data
tumorData = tumorData[,2:10]
#convert the item with NA to mean value from the column rounded up
f=function(x){
   x[is.na(x)] =round(mean(x, na.rm=TRUE))
   x #returns the column
}
#applys the function f to all columns in the tumor data
tumorData=data.frame(apply(tumorData,2,f))

#************** PCA ****************#
#runs PCA on tumor data
tumorFeaturesPCA = prcomp(tumorData)
#data projection onto the first and second principle components
xTumorPCA = tumorFeaturesPCA$x[,1]
yTumorPCA = tumorFeaturesPCA$x[,2]

#************** MDS ****************#
#generates a distance matrix of the data
tumorFeaturesMDS = dist(tumorData)
#runs MDS on the distance matrix
tumorFeaturesMDS = cmdscale(tumorFeaturesMDS)

#sets vectors for the plots
xTumorMDS = tumorFeaturesMDS[,1]
yTumorMDS = tumorFeaturesMDS[,2]

#************** LDA ****************#

#sets tumor classes based on maligancy
tumorClasses[which(tumorClasses == 2)] = "benign"
tumorClasses[which(tumorClasses == 4)] = "malignant"
 
#random selection of for about 80% of the samples
trainingIndexT = sample(1:698,558)
#runs LDA on tumor data, (Note: there is only one linear discriminat)
tumorLDA = lda(tumorData[trainingIndexT,], tumorClasses[trainingIndexT])
#attempts to classify the remaining samples based on the results from
#the LDA analysis
classifyT = predict(tumorLDA, tumorData[-trainingIndexT, ])$class
#number of incorrectly identified samples
accuracyT = classifyT[which(classifyT != tumorClasses[-trainingIndexT])]
#prints a statement about overall classification accuracy
paste("Classification accuracy was ", 
round(100*(length(tumorClasses)-length(accuracyT))/length(tumorClasses), 2), "%.", sep ="")
#Projects data onto first linear discriminates
xTumorLDA = as.matrix(tumorData) %*% tumorLDA$scaling[,1]
xTumorLDABenign = as.matrix(tumorData[which(tumorClasses == "benign"),]) %*% tumorLDA$scaling[,1]
xTumorLDAMalignant = as.matrix(tumorData[which(tumorClasses == "malignant"),]) %*% tumorLDA$scaling[,1]
#yTumorLDA = as.matrix(fruitData) %*% fruitLDA$scaling[,2]

#################Tumor Comparison Plots###################

#sets density curves for the LDA data
dens = density(xTumorLDA)
densBenign = density(xTumorLDABenign)
densMalignant = density(xTumorLDAMalignant)
#sets range for LDA plot
xRange = range(dens$x)

#creates a color vector for the data points, colorizes by class
colorVectorTumor = as.vector(tumorClasses)
colorVectorTumor[which(tumorClasses == "benign")] = "blue"
colorVectorTumor[which(tumorClasses == "malignant")] = "red"


#sets up a plotting window with three subplots, one for each method of analysis
windows(6.6,2.2)
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)

par(mar = c(2.5,2.5,1.,1), mgp = c(1.5,.5,0))

#plots the data projected onto the first linear discriminate
plot(densBenign$x, densBenign$y, type = "l", col = "blue", 
	xlim = xRange, xlab = "Linear Discriminant 1", ylab = "Density")
lines(densMalignant$x, densMalignant$y, col="red")
mtext("(A)", line = -2)

#plots the data for the first and second principle coordinates
plot(-1*xTumorMDS,-1*yTumorMDS, xlab = "Principle Coordinate 1", ylab = "Principle Coordinate 2", pch = 20, col = colorVectorTumor)
mtext("(B)", line = -2)

#plots the projected data against the first and second
#principle components
plot(xTumorPCA,-1*yTumorPCA, xlab = "Principal Component 1", ylab = "Principal Component 2", pch = 20, col = colorVectorTumor)
mtext("(C)", line = -2)

