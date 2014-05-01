################################################################
#Nathan Sponberg
#Lab 2 - Functions, Classes, File I/O
#09/05/2013
#
#file_io.py
################################################################

import csv

def read(filePath):
	filePath = filePath
	results = []
	#input("Enter the path of the file to be read: ")
	from csv import reader
	fileInput = open(file = filePath, mode = "r", errors="?")
	parser = reader(fileInput)
	for x, y in parser:
		results.append((x,y))
	fileInput.close()
	return results
	#return fileInput
	
def writeScoreCSV(inputList,filePath):
    with open(filePath, "w+") as csvfile:
        rowWriter = csv.writer(csvfile, delimiter=",")
        for item in inputList:
            rowWriter.writerow([item[0],item[1]])

def readScoreCSV(filePath):
    scoreInput = []
    with open(filePath, "r") as csvfile:
        parser = csv.reader(csvfile)
        for row in parser:
            scoreInput.append((row[0],row[1]))
    return scoreInput
    
	
def writer(inputList, filePath):
	inputList = inputList
	filePath = filePath
	fileOutput = open(filePath, "w+")
	fileOutput.writelines(str(x) + "\n" for x in inputList)
	fileOutput.flush()
	fileOutput.close()
