#############Feature Selection Project#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 3/24/2013
#
#Selection of feature selection demonstrations
#
#####################################################

setwd("C:\\Users\\Pip\\Documents\\CS448\\FeatureSelectionProject")
source('FeatureSelectionFunctions.r')
library(MASS)

###################T Tests####################

#reads in iris data
rawData = iris
#pulls out the numeric data from the iris set
numericData = rawData[1:4]
#class vector for iris data
irisClasses = iris$Species

#get ttest scores
irisBar = Tscores(numericData,irisClasses)

#barplot for ttest iris
windows(6,4)
barplot(irisBar,names.arg = colnames(numericData), ylim = c(0,35),
		xlab = 'Classes', ylab = 'T-Test Statistic Average')
		
###########Iris LDA##########

#randomly picks about 75% of samples from the iris data as the training set
#for the LDA classifier
trainingIndex = sample(1:150,112)
#runs LDA on the training set
irisLDANormalDims = lda(numericData[trainingIndex,], irisClasses[trainingIndex])
#attempts to classify the remaing 25 samples based on the results from
#the LDA analysis
classify = predict(irisLDANormalDims, numericData[-trainingIndex, ])$class
#number of incorrectly identified samples
accuracy = classify[which(classify != irisClasses[-trainingIndex])]
#prints a statement about overall classification accuracy
print(paste("Classification accuracy for normal dimensions on iris data was ", round(100*(25-length(accuracy))/25, 2), "%.", sep =""))

xIrisLDANorm = as.matrix(numericData) %*% irisLDANormalDims$scaling[,1]
yIrisLDANorm = -1*as.matrix(numericData) %*% irisLDANormalDims$scaling[,2]

#randomly picks about 75% of samples from the iris data as the training set
#for the LDA classifier
trainingIndex = sample(1:150,112)
#runs LDA on the training set
irisLDAReducedDims = lda(numericData[trainingIndex,which(irisBar >= 20)], irisClasses[trainingIndex])
#attempts to classify the remaing 25 samples based on the results from
#the LDA analysis
classify = predict(irisLDAReducedDims, numericData[-trainingIndex,which(irisBar >= 20)])$class
#number of incorrectly identified samples
accuracy = classify[which(classify != irisClasses[-trainingIndex])]
#prints a statement about overall classification accuracy
print(paste("Classification accuracy for reduced dimensions on iris data was ", round(100*(25-length(accuracy))/25, 2), "%.", sep =""))

xIrisLDAReduce = as.matrix(numericData[,which(irisBar >= 20)]) %*% irisLDAReducedDims$scaling[,1]
YIrisLDAReduce = as.matrix(numericData[,which(irisBar >= 20)]) %*% irisLDAReducedDims$scaling[,2]

################Fruit LDA###################

#reads in the fruit data as a data.frame
fruitInput = read.csv("fruit.csv")
#pulls the data points out of the fruit data
fruitData = fruitInput[,1:4]
fruitTypes = fruitInput[,5]

#get ttest scores
fruitBar = Tscores(fruitData,fruitTypes)

#barplot for ttest fruit
windows(6,4)
barplot(fruitBar,names.arg = colnames(fruitData), ylim = c(0,70),
		xlab = 'Classes', ylab = 'T-Test Statistic Average')
		

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
print(paste("Classification accuracy for normal dimension on fruit data was ", 
round(100*(length(fruitTypes)-length(accuracyF))/length(fruitTypes), 2), "%.", sep =""))

#random selection of 800 samples from the fruit data( out of 1000 total)
trainingIndexF = sample(1:1000,800)
#runs LDA on fruit data
fruitLDAReduce = lda(fruitData[trainingIndexF,which(fruitBar >= 20)], fruitTypes[trainingIndexF])
#attempts to classify the remaining 200 samples based on the results from
#the LDA analysis
classifyF = predict(fruitLDAReduce, fruitData[-trainingIndexF, which(fruitBar >= 20)])$class
#number of incorrectly identified samples
accuracyF = classifyF[which(classifyF != fruitTypes[-trainingIndexF])]
#prints a statement about overall classification accuracy
print(paste("Classification accuracy for reduced dimension on fruit data was ", 
round(100*(length(fruitTypes)-length(accuracyF))/length(fruitTypes), 2), "%.", sep =""))

###############Tumor LDA####################

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

#sets tumor classes based on maligancy
tumorClasses[which(tumorClasses == 2)] = "benign"
tumorClasses[which(tumorClasses == 4)] = "malignant"

#get ttest scores
tumorBar = Tscores(tumorData,tumorClasses)

#barplot for ttest fruit
windows(6,4)
barplot(tumorBar,names.arg = colnames(tumorData), ylim = c(0,35),
		xlab = 'Classes', ylab = 'T-Test Statistic Average')

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
print(paste("Classification accuracy for normal dimensions on tumor data was ", 
round(100*(length(tumorClasses)-length(accuracyT))/length(tumorClasses), 2), "%.", sep =""))

#random selection of for about 80% of the samples
trainingIndexT = sample(1:698,558)
#runs LDA on tumor data, (Note: there is only one linear discriminat)
tumorLDAReduce = lda(tumorData[trainingIndexT,which(tumorBar >= 20)], tumorClasses[trainingIndexT])
#attempts to classify the remaining samples based on the results from
#the LDA analysis
classifyT = predict(tumorLDAReduce, tumorData[-trainingIndexT,which(tumorBar >= 20) ])$class
#number of incorrectly identified samples
accuracyT = classifyT[which(classifyT != tumorClasses[-trainingIndexT])]
#prints a statement about overall classification accuracy
print(paste("Classification accuracy for reduced dimensions on tumor data was ", 
round(100*(length(tumorClasses)-length(accuracyT))/length(tumorClasses), 2), "%.", sep =""))

#################Mouse LDA##################

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

#get ttest scores
mouseBar = Tscores(mouseTranspose,mouseClasses)

#barplot for ttest fruit
windows(7,4)
barplot(mouseBar, ylim = c(0,3.0), names.arg = c(1:98),
		xlab = 'Classes', ylab = 'T-Test Statistic Average')

#takes only most significant mouse dimensions
reducedMouseBar = mouseBar[which(mouseBar >= 1.78)]

crossValidationValues = rep(0,nrow(mouseTranspose))

for(s in 1:nrow(mouseTranspose)){
	trainingIndex = which(c(1:nrow(mouseTranspose)) != s)
	LDA = lda(mouseTranspose[trainingIndex,which(mouseBar >= 1.78)], mouseClasses[trainingIndex])
	classifyT = predict(LDA, mouseTranspose[s,which(mouseBar >= 1.78)])$class
	#print(classifyT)
	#number of incorrectly identified samples
	accuracyT = classifyT[which(classifyT != mouseTranspose[-trainingIndex])]
	if(classifyT == mouseClasses[s]){
		crossValidationValues[s] = 1
	}else
		crossValidationValues[s] = 0
}

#################Mouse Plot###################
#sets up vector for the colorization of the data points, sorts by
#whether sample was proximal or distal
colorVectorMouse = as.vector(experimentIDs)
colorVectorMouse[proxIndex] = "blue"
colorVectorMouse[-proxIndex] = "green"
#runs LDA on mouse data
mouseLDA = lda(mouseTranspose[,which(mouseBar >= 1.78)], mouseClasses, tol=0)


xMouseLDA = as.matrix(mouseTranspose[,which(mouseBar >= 1.78)]) %*% mouseLDA$scaling[,1]
#yMouseLDA = as.matrix(mouseTranspose[,which(mouseBar >= 1.78)]) %*% mouseLDA$scaling[,2]

#Project mouse data
xMouseProximal = as.matrix(mouseTranspose[which(mouseClasses == "proximal"),which(mouseBar >= 1.78)]) %*% mouseLDA$scaling[,1]
xMouseDistal = as.matrix(mouseTranspose[which(mouseClasses == "distal"),which(mouseBar >= 1.78)]) %*% mouseLDA$scaling[,1]

dens = density(xMouseLDA)
densProx = density(xMouseProximal)
densDist = density(xMouseDistal)
xRange = range(dens$x)

windows(6,4)
par(mar = c(2.5,2.5,1.,1), mgp = c(1.5,.5,0))

plot(densProx$x, densProx$y, type = "l", col = "blue", 
	xlim = xRange, xlab = "Linear Discriminant 1", ylab = "Density")
lines(densDist$x, densDist$y, col="red")
legend("topleft", legend = c("Proximal","Distal"), col = c("red","blue"),pch = 20)

##############################################



#barplot(yBar, names.arg = xBar, main = "Porportions of Variance (Iris)")