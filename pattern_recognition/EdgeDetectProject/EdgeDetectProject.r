#############Edge Detection Project#############
#By: Nathan Sponberg
#For: CS488-Pattern Recognition 
#Spring 2013
#Started: 5/8/2013
#
#Project exploring edge detection algorithms
#
#####################################################

setwd("C:\\Users\\Pip\\Documents\\CS448\\EdgeDetectProject")
source("EdgeDetectFunctions.r")
library(biOps)

############Flower ###############

violet = readJpeg(system.file("samples", "violet.jpg", package="biOps"))

###sobel plot
par(mar=c(0,0,0,0))
plot(imgNegative(imgSobel(violet)))

###canny plot
dev.new()
par(mar=c(0,0,0,0))
plot(imgCanny(violet, 1.4))

##############Lenna##############

lenna = readJpeg("Lenna.jpeg")

###laplace plot-normal boundries
dev.new()
par(mar=c(0,0,0,0))
plot(LaplaceEdge(lenna,5,0))

###gaussian convolution
lennaBlur = GaussianBlur(lenna,5,1.4)

dev.new()
par(mar=c(0,0,0,0))
plot(LaplaceEdge(lennaBlur,5,0))

###boundry change
dev.new()
par(mar=c(0,0,0,0))
plot(LaplaceEdge(lennaBlur,5,100))

###canny plot
dev.new()
par(mar=c(0,0,0,0))
plot(imgCanny(lenna,1.4))