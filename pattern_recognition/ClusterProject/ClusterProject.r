#############Cluster Project#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 4/18/2013
#
#Project exploring various methods of clustering data
#
#####################################################

#setwd('C:\\Users\\Pip\\Documents\\CS448\\ClusterProject')
library(mlbench)
source('ClusterFunctions.r')
library(gplots)

##############Smiley Data################

smileyData = mlbench.smiley()

#plot(smileyData$x)

###K-Means

kmeanSmiley = kmeans(smileyData$x, 4)

colorVector = rep(nrow(smileyData$x):1)
colorVector[which(kmeanSmiley$cluster == 1)] = "green"
colorVector[which(kmeanSmiley$cluster == 2)] = "red"
colorVector[which(kmeanSmiley$cluster == 3)] = "blue"
colorVector[which(kmeanSmiley$cluster == 4)] = "black"

windows(4,4.5)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
plot(smileyData$x, xlab = 'X-Axis', ylab = 'Y-Axis', pch = 20, col = colorVector)

###HClust

hclustSmiley = dist(smileyData$x)
hclustSmiley = hclust(hclustSmiley)
membershipVecFour = cutree(hclustSmiley,k=4)

colorVector[which(membershipVecFour == 1)] = "green"
colorVector[which(membershipVecFour== 2)] = "red"
colorVector[which(membershipVecFour == 3)] = "blue"
colorVector[which(membershipVecFour == 4)] = "black"

windows(4,4.5)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
plot(smileyData$x, xlab = 'X-Axis', ylab = 'Y-Axis', pch = 20, col = colorVector)

###Ward

hclustSmiley = dist(smileyData$x)
hclustSmiley = hclust(hclustSmiley, method = 'ward')
membershipVecFour = cutree(hclustSmiley,k=4)

colorVector[which(membershipVecFour == 1)] = "green"
colorVector[which(membershipVecFour== 2)] = "red"
colorVector[which(membershipVecFour == 3)] = "blue"
colorVector[which(membershipVecFour == 4)] = "black"

windows(4,4.5)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
plot(smileyData$x, xlab = 'X-Axis', ylab = 'Y-Axis', pch = 20, col = colorVector)



hclustSmiley = dist(smileyData$x)
hclustSmiley = hclust(hclustSmiley, method = 'ward')
membershipVecFour = cutree(hclustSmiley,k=12)

colorVector[which(membershipVecFour == 1)] = "green"
colorVector[which(membershipVecFour== 2)] = "red"
colorVector[which(membershipVecFour == 3)] = "blue"
colorVector[which(membershipVecFour == 4)] = "black"
colorVector[which(membershipVecFour == 5)] = "purple"
colorVector[which(membershipVecFour == 6)] = "yellow"
colorVector[which(membershipVecFour == 7)] = "turquoise"
colorVector[which(membershipVecFour == 8)] = "orange"
colorVector[which(membershipVecFour == 9)] = "burlywood1"
colorVector[which(membershipVecFour == 10)] = "wheat4"
colorVector[which(membershipVecFour == 11)] = "steelblue1"
colorVector[which(membershipVecFour == 4)] = "yellowgreen"

windows(4,4.5)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
plot(smileyData$x, xlab = 'X-Axis', ylab = 'Y-Axis', pch = 20, col = colorVector)

#####################BSAS#######################

irisData = iris[,1:4]
distMatrix = dist(irisData)
minDist = min(distMatrix)
maxDist = max(distMatrix)
if(minDist == 0){
	minDist = 0.1
}

##BSAS Loop

thetaRange = seq(minDist,maxDist, length.out = 100)

numClusters = c()

for(theta in thetaRange){
	clusters = Bsas(irisData,theta,nrow(irisData))
	numClusters = c(numClusters, length(clusters))
}

windows(4,4.5)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
plot(thetaRange,numClusters, xlab = "Theta", ylab = "Number of Clusters", pch = 20, xlim = c(1,7.5), ylim = c(0,10))

irisClustersThree = Bsas(irisData,2.2,3)

#takes the column containing the species associated with each
#data point, used later to colorize the plot
irisSpecies = iris$Species
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

colorVector2 = as.vector(irisSpecies)

colorVector2[irisClustersThree[[1]]] = "red"
colorVector2[irisClustersThree[[2]]] = "green"
colorVector2[irisClustersThree[[3]]] = "black"

#################Iris Data Plots####################
windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the projected data against the first and second
#principle components
plot(x,y, xlab = "Principal Component 1", ylab = "Principal Component 2",
	 pch = 20, col = colorVector)
legend("topright", levels(irisSpecies), col = c("red","green","blue"),pch = 20)
#calculates the stadard deviation for the principle components

windows(4,4)
par(mar = c(2.5,2.5,2.5,.1), mgp = c(1.5,.5,0))
#plots the projected data against the first and second
#principle components
plot(x,y, xlab = "Principal Component 1", ylab = "Principal Component 2",
	pch = 20, col = colorVector2)
legend("topright", levels(irisSpecies), col = c("red","green","black"),pch = 20)
#calculates the stadard deviation for the principle components

#############Gene Expression Data###################

geneData = read.csv("ecoliDiffExpReduced.csv")
geneData = geneData[3:6]
geneData = as.matrix(geneData)
windows(4,4)
heatmap(geneData, Colv=NA, col = redgreen(75), labRow=NA, cexRow = 0.2)

geneClusters = hclust(dist(geneData))
memberVectorThree = cutree(geneClusters, k=3)

featureData = prcomp(geneData)

x = featureData$x[,1]
y = featureData$x[,2]

colorVector = as.vector(geneData[,1])

colorVector[which(memberVectorThree == 1)] = "green"
colorVector[which(memberVectorThree == 2)] = "black"
colorVector[which(memberVectorThree == 3)] = "orange"

windows(4,4)
par(mar = c(2.5,2.5,2.5,1), mgp = c(1.5,.5,0))
#plots the projected data against the first and second
#principle components
plot(x,y, xlab = "Principal Component 1", ylab = "Principal Component 2",
	pch = 20, col = colorVector)

windows(10,3)
par(mfrow = c(1,3))
par(mar=c(2.5,3,.1,.1),mgp = c(1.5,.5,0))
layout(matrix(c(1:3),1,3), widths = c(1,1,1))
beg = range(geneData)[1]
theEnd = range(geneData)[2]
for(i in c(1:3)){
	rows = which(memberVectorThree == i)
	clustSize = length(rows)
	xvals = c(0:(clustSize*4-1))
	xvals = xvals %%4+1
	xvals = matrix(xvals,ncol=4,byrow=TRUE)
	par(mfg=c(1,i))
	if(i==1){
		plot(xvals[1,],geneData[rows[1],], ylab = "Expression Level", xlab = "", type = "l", ylim = range(geneData),xaxt='n',yaxt='n')
	}
	if(i==2){
		plot(xvals[1,],geneData[rows[1],], ylab = "", xlab = "Time", type = "l", ylim = range(geneData),xaxt='n',yaxt='n')
	}
	if(i==3){
		plot(xvals[1,],geneData[rows[1],], ylab = "", xlab = "", type = "l", ylim = range(geneData),xaxt='n',yaxt='n')
	}
	for(j in c(2:clustSize)){
		lines(xvals[j,],geneData[rows[j],])
	}
	if(i == 1){
		axis(2,at = floor(seq(beg,theEnd,length.out = 4)))
	}
	axis(1,at = c(1,2,3,4))
}


#vectorData = c(0,9.5,10.5,9.5,10.5,9.5,0,3,6,7,10.5,3,0,7,8,9.5,6,7,0,3,10.5,7,8,3,0)
#matrixData = matrix(vectorData,ncol=5,byrow=TRUE)
#clusters = hclust(as.dist(matrixData))
#dendro = as.dendrogram(clusters)

##############CPCC###############

#irisData = iris[,1:4]
#randSets = RandDataSets(irisData, 10000)
#CPCCScores = c(1:10000)
#for(i in c(1:10000)){
#	CPCCScores[i] = GetCPCC(randSets[[i]])
#}

#irisCPCC = GetCPCC(irisData)
#numCPCCs = CPCCScores[which(CPCCScores >= irisCPCC)]
#probIrisRand = length(numCPCCs)/10000
#windows(4,4)
#hist(CPCCScores)
