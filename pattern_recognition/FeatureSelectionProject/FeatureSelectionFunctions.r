#############Feature Selection Functions#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 3/24/2013
#
#functions used for feature selection
#
#####################################################

#assumes classes is a 'factor' data type of what class correspondes to
#each sample point in the data matrix. Data matrix is assumed to
#have rows as sample points and columns as dimension.

Tscores = function(dataMatrix, classes){
	classes = classes
	classTypes = unique(classes)
	numCol = ncol(dataMatrix) #DO NOT include a class vector in this matrix!!
	numClasses = length(classTypes)
	TScores = rep(0,numCol)
	for(D in 1:numCol){
		#currentDim = dataMatrix[,D]
		TScoreTotal = 0.
		numScores2 = 0
		for(i in 1:(numClasses-1)){
			#print(i)
			for(j in (i+1):numClasses){
				class1Dat = dataMatrix[which(classes == classTypes[i]),D]
				class2Dat = dataMatrix[which(classes == classTypes[j]),D]
				currentTest = t.test(class1Dat,class2Dat)
				TScoreTotal = TScoreTotal + abs(currentTest$statistic)
			}
		}
		numScores = (numClasses*(numClasses-1))/2
		avgTScore = TScoreTotal/numScores
		TScores[D] = avgTScore
	}
	return(TScores)
}

Sw = function(dataMatrix, classes){
	classTypes = unique(classes)
	numSamples = nrow(dataMatrix)
	numClasses = length(classTypes)
	SwDim = ncol(dataMatrix)
	Sw = mat.or.vec(SwDim,SwDim)
	for(C in 1:numClasses){
		subSet = dataMatrix[which(classes == classTypes[C]),]
		P = nrow(subSet)/numSamples
		subSetSw = P*cov(subSet)
		Sw = Sw + subSetSw
	}
	return(Sw)
}

Sb = function(dataMatrix,classes){
	classTypes = unique(classes)
	numSamples = nrow(dataMatrix)
	numClasses = length(classTypes)
	SbDim = ncol(dataMatrix)
	Sb = mat.or.vec(SbDim,SbDim)
	globalColMeans = colMeans(dataMatrix)
	for(C in 1:numClasses){
		subSet = dataMatrix[which(classes == classTypes[C]),]
		P = nrow(subSet)/numSamples
		subSetMeans = colMeans(subSet)
		Sb = Sb + (P * ((subSetMeans - globalColMeans) %*% t(subSetMeans - globalColMeans)))
	}
	return(Sb)
}

J1 = function(dataMatrix,classes){
	SbMatrix = Sb(dataMatrix,classes)
	SwMatrix = Sw(dataMatrix,classes)
	numer = sum(diag(SbMatrix+SwMatrix))
	demon = sum(diag(SwMatrix))
	J1Score = numer/demon
	return(J1Score
	)
}

J2  = function(dataMatrix,classes){
	SbMatrix = Sb(dataMatrix,classes)
	SwMatrix = Sw(dataMatrix,classes)
	SmMatrix = SbMatrix+SwMatrix
	J2Score = det(solve(SwMatrix) %*% SmMatrix)
	return(J2Score)
}

J3  = function(dataMatrix,classes){
	SbMatrix = Sb(dataMatrix,classes)
	SwMatrix = Sw(dataMatrix,classes)
	J3Score = sum(diag(solve(SwMatrix) %*% SbMatrix))
	return(J3Score)
}



FloatingSearch = function(dataMatrix, classes, desiredDims){
	scoreList = list()
	dimList = list()
	remainingDims = (1:ncol(dataMatrix))
	k = 0
	while(k <= (desiredDims)){
		#Find and score (e.g. with J3)
		if(k == 0){
			scores = Tscores(dataMatrix, classes)
			bestDim = which.max(scores)
			scoreList[[k+1]] = scores[bestDim]
			dimList[[k+1]] = bestDim
			remainingDims = remainingDims[remainingDims != bestDim]
			k = k+1
			print(dimList)
			print(remainingDims)
			print(scoreList)
			print("--------------------")
		}else if(k == 1){
			kPlusBestScore = 0 #best k+1 score, updates as following loop iterates
			nextBestDims = dimList[[k]]
			kPlusDim = 0
			for(D in remainingDims){
				testDims = c(dimList[[k]],D)
				testScore = J3(dataMatrix[,testDims],classes)
				if(testScore > kPlusBestScore){
					kPlusBestScore = testScore
					nextBestDims = testDims
					kPlusDim = D
				}
			}
			dimList[[k+1]] = nextBestDims
			scoreList[[k+1]] = kPlusBestScore
			remainingDims = remainingDims[remainingDims != kPlusDim]
			print(dimList)
			print(remainingDims)
			print(scoreList)
			print("--------------------")
			k = k+1
		}else{
			kPlusBestScore = 0 #best k+1 score, updates as following loop iterates
			nextBestDims = dimList[[k]]
			kPlusDim = 0
			for(D in remainingDims){
				testDims = c(dimList[[k]],D)
				testScore = J3(dataMatrix[,testDims],classes)
				if(testScore > kPlusBestScore){
					kPlusBestScore = testScore
					nextBestDims = testDims
					kPlusDim = D
				}
			}
			dimList[[k+1]] = nextBestDims
			scoreList[[k+1]] = kPlusBestScore
			remainingDims = remainingDims[remainingDims != kPlusDim]
			print(dimList)
			print(remainingDims)
			print(scoreList)
			print("--------------------")
			#all sets of size k+1 features, 
			#keep best as set k+1
			leastEffectScore = 0 #best k prime score, updates as following loop iterates
			leastEffectDim = 0
			kPrimeDims = dimList[[k+1]]
			for(D in dimList[[k+1]]){
				testDims = dimList[[k+1]]
				testDims = testDims[testDims != D]
				testScore = J3(dataMatrix[,testDims],classes)
				if(testScore > leastEffectScore){
					leastEffectScore = testScore
					kPrimeDims = testDims
					leastEffectDim = D
				}
			}
			#Find feature that has the least effect when removed
			if(kPlusDim == leastEffectDim){
				#k = k+1 and loop
				k = k+1
				print("-------1-------------")
			}else if(leastEffectScore <= scoreList[[k]]){
				#k=k+1 and loop
				k = k+1
				print("-------2-------------")
			}else if(k == 2){
			  #replace set with k+1 less least 
			  #affecting feature, loop
			  remainingDims = c(remainingDims,leastEffectDim)
			  scoreList[[k+1]] = NULL
			  dimList[[k+1]] = NULL
			  dimList[[k]] = kPrimeDims
			  scoreList[[k]] = leastEffectScore
			  print("--------3------------")
			}else{
				scoreList[[k+1]] = NULL
				dimList[[k+1]] = NULL
				dimList[[k]] = kPrimeDims
				scoreList[[k]] = leastEffectScore
				remainingDims = c(remainingDims,leastEffectDim)
				improved = TRUE
				print("-------4-------------")
				while(k>2 && improved ){
					#Replace k set with improved set (k+1 less 
					#least contributor)
					dimList[[k]] = kPrimeDims
					#Find least affecting feature in new set k
					leastEffectScore = 0 #best k prime score, updates as following loop iterates
					leastEffectDim = 0
					kPrimeDims = dimList[[k]]
					for(D in dimList[[k]]){
						testDims = dimList[[k]]
						testDims = testDims[testDims != D]
						testScore = J3(dataMatrix[,testDims],classes)
						if(testScore > leastEffectScore){
							leastEffectScore = testScore
							kPrimeDims = testDims
							leastEffectDim = D
						}
					}
					if(leastEffectScore <= scoreList[[k-1]]){
						#If removing it does not improve score of k-1exit 
						#this loop
						improved  = FALSE
					}else{
						#Set k-1 to the new set, and recalculate cost and set k = k-1
						scoreList[[k]] = NULL
						dimList[[k]] = NULL
						dimList[[k-1]] = kPrimeDims
						scoreList[[k-1]] = leastEffectScore
						remainingDims = c(remainingDims,leastEffectDim)
						k= k-1
					}	
				}
			}
		}
	}
	return(dimList[desiredDims])
}