#############Edge Detect Functions#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 3/24/2013
#
#functions used for edge detection
#
#####################################################

setwd("C:\\Users\\Pip\\Documents\\CS448\\EdgeDetectProject")

###expects a square matrix with odd dimensions
LaplacianConvolutionMask = function(matrixData){
	matrixData = matrixData
	matrixDim = ncol(matrixData)
	mask = matrix(-1, matrixDim, matrixDim)
	mask[ceiling(matrixDim/2),ceiling(matrixDim/2)] = matrixDim*matrixDim-1
	return(mask)
}

LaplaceEdge = function(imageData, maskResolution, lowerBound){
	maskRes = maskResolution
	imageData = imageData
	if(length(dim(imageData)) >= 3){
		imageData = imgRGB2Grey(imageData)
	}
	outputData = imageData
	rows = nrow(imageData)
	columns = ncol(imageData)
	borderWidth = maskRes
	for(R in c(1:(rows - borderWidth))){
		for(C in c(1:(columns - borderWidth))){
			imgWindow = imageData[R:(R+maskRes-1),C:(C+maskRes-1)]
			windowMask = LaplacianConvolutionMask(imgWindow)
			convolutedData = windowMask*imgWindow
			#convolutedData = -1*convolutedData + 255
			value = sum(convolutedData)
			#value = 255 - value
			if(value >= 255){
				outputData[R+floor(maskRes/2),C+floor(maskRes/2)] = 255
			}else if(value <= lowerBound){
				outputData[R+floor(maskRes/2),C+floor(maskRes/2)] = 0
			}else{
				outputData[R+floor(maskRes/2),C+floor(maskRes/2)] = value
			}
			
		}
	}
	outputData = imgNegative(outputData)
	return(outputData)
}

GaussianWeight = function(r, sigma){
	return(exp(-(r**2)/(2*sigma**2)))
}

PixelDist = function(matrixDim){
	distMatrix = matrix(0,matrixDim,matrixDim)
	center = ceiling(matrixDim/2)
	for(R in c(1:(matrixDim))){
		for(C in c(1:(matrixDim))){
			distMatrix[R,C] = abs(sqrt((R-center)**2 + (C-center)**2))
		}
	}
	return(distMatrix)
}

GaussianMask = function(matrixDim, sigma){
	qVals = PixelDist(matrixDim)
	mask = round(15*GaussianWeight(qVals, sigma))
	return(mask)
}

GaussianBlur = function(imageData, maskResolution, sigma){
	maskRes = maskResolution
	imageData = imageData
	if(length(dim(imageData)) >= 3){
		imageData = imgRGB2Grey(imageData)
	}
	outputData = imageData
	rows = nrow(imageData)
	columns = ncol(imageData)
	borderWidth = maskRes
	for(R in c(1:(rows - borderWidth))){
		for(C in c(1:(columns - borderWidth))){
			imgWindow = imageData[R:(R+maskRes-1),C:(C+maskRes-1)]
			windowMask = GaussianMask(maskRes, sigma)
			windowMask = -1*windowMask
			windowMask[ceiling(maskRes/2),ceiling(maskRes/2)] = -1*windowMask[ceiling(maskRes/2),ceiling(maskRes/2)]
			convolutedData = (windowMask*imgWindow)/sum(windowMask)
			#convolutedData = -1*convolutedData + 255
			value = sum(convolutedData)
			#value = 255 - value
		
			outputData[R+floor(maskRes/2),C+floor(maskRes/2)] = value
			
			
		}
	}
	return(outputData)
}