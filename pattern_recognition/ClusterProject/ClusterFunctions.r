#############Clustering Functions#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 3/24/2013
#
#functions used for clustering
#
#####################################################

###Basic Sequential Algorithm###

#dataMatrix should contain only numeric data
Bsas = function(dataMatrix, theta, numClust){
	theta = theta
	numClust = numClust
	dataSet = dataMatrix
	remainingDataPoints = dataMatrix
	clust = 1
	clusters = list()
	clusterMeans = list()
	clusters[[1]] = (1)
	clusterMeans[[1]] = dataSet[1,]
	#print("ok")
	for(S in c(2:nrow(dataSet))){
		distVector = c()
		#print("ok2")
		for(M in c(1:length(clusters))){
			#print(clusterMeans)
			distVector[M] = sqrt(sum((clusterMeans[[M]] - dataSet[S,])^2))
			
		}
		#print("ok3")
		closestClust = which(distVector == min(distVector))
		if(distVector[closestClust] > theta && clust < numClust){
			clust = clust + 1
			clusters[[clust]] = S
			clusterMeans[[clust]] = dataSet[S,]
		}
		else{
			clusters[[clust]] = c(clusters[[clust]], S)
			clustSize = length(clusters[[clust]])
			clusterMeans[[clust]] = ((clustSize - 1)*clusterMeans[[clust]] + dataSet[S,])/clustSize
			#print(clusterMeans[[clust]])
		}
	}
	return(clusters)
}

RandDataSets = function(givenDataMatrix, numRandSets){
	sampleData = givenDataMatrix
	sampleSize = nrow(sampleData)
	numSets = numRandSets
	vectorSize = ncol(givenDataMatrix)
	dimRanges = list()
	for(i in c(1:vectorSize)){
		dimRanges[[i]] = range(sampleData[,i])
	}
	randomSets = list()
	for(S in c(1:numSets)){
		onesVector = rep(1, sampleSize*vectorSize)
		sampleMatrix = matrix(onesVector, ncol = vectorSize, byrow = TRUE)
		for(C in c(1:vectorSize)){
			currentCol = runif(sampleSize, min = dimRanges[[C]][1], max = dimRanges[[C]][2])
			sampleMatrix[,C] = currentCol
		}
		randomSets[[S]]=sampleMatrix
	}
	return(randomSets)
}

GetCPCC = function(dataMatrix){
	d1 = dist(dataMatrix)
	hc = hclust(d1)
	d2 = cophenetic(hc)
	return(cor(d1,d2iris))
}