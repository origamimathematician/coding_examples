#######################################
# Nathan Sponberg
# Project 5 - Police Database Simulation
#
# 12/02/13
#
# simulation.py
#--------------------------------------
# Creates a simulated police database that
# runs over a simulated time interval.
# Program creates all random data and simulates
# random interactions with the database.
#######################################

import csv
from bst import BinarySearchTree
from random import randrange, randint, shuffle
import random
import datetime
from timeit import Timer
from namereader import readNames
import copy
from sys import setrecursionlimit
####For plotting functions is benchmark methods
#import matplotlib.pyplot as plt 

### List of possible criminal offences, can be expanded or reduced.
### Offences are chosen random.
OFFENCES = ["Arson", "Assault", "Breaking and Entering", "Drunk Driving", "Grand Theft Auto", "Identity Theft", "Intent to Distribute", "Money Laundering", "Possession", "Shoplifting", "Tax Evasion", "Theft", "Vandalism"]

### constants for the end points of the time interval for the simulation
STARTDATE = '1976-1-1 00:00'
ENDDATE = '2014-1-1 00:00'

#######Because timeit f***ing hates me########
    #used for benchmarking
nameList = readNames("nameseed3000.txt")
benchmarkSim = None
benchmarkTime = None
benchmarkPerson = None
##############################################

### Conditional, determines whether the simulation will run or if benchmarks
### will be run on database operations
runBenchmarks = False
plotBenchmarks = False ###WARNING: only works for me using python 2.7, untested 
                         # with python 3.x

################################################################################
#
# This class represents a simulation of the police database. The simulation can
# be started with a built in class method.
#
################################################################################

class CrimeDBSim():
    
    def __init__(self, nameList, preLoadPercent = .25, logger = None):
        self.names = nameList
        self.DB = CriminalDataBase()    #database object
        shuffle(self.names) #shuffle names so that order is different for each
                            #simulation
                            
        ### these are that names that will be preloaded into the database.
        ### This represents the preexisting data in the database before the
        ### the simulation starts.
        self.preLoadNames = self.names[:int(len(self.names)*preLoadPercent)]
        
        ### used for random retrieval and deletion
        self.inDataBase = self.preLoadNames
        
        ### unused names, for preforming random actions during sim
        self.remainingNames = self.names[int(len(self.names)*preLoadPercent)+1:]
        
        ### load database with preexisting random data
        for name in self.preLoadNames:
            newRec = Record()
            # names with random record to database
            self.DB.put(name, newRec)
            
        ### initalize action logging
        self.logging = False
        self.logger = logger
        if logger != None:
            self.logging = True
        
    ### This method will begin the simulation itself, all actions are randomly
    ### generated using builtin methods.
        # startTime gives the date when the simulation will start
        # iterations sets the number of days the simulation runs
        # deletions determines if deletions are a valid random event
    def runSim(self, startTime = ENDDATE, iterations = 1000, deletions = True):
        currentDay = convertDateString(startTime)   #start day
        ### sets a 24 hour period
        nextDay = currentDay + datetime.timedelta(days=1)
        
        ### open log if logging is taking place
        if self.logging:
                self.logger.openLog()        
        
        ### main simulation loop
        for day in range(iterations):
        
            ### console output
            logEntry = "\n#####"
            logEntry += "Database Log for " + currentDay.strftime("%Y-%m-%d")
            logEntry += "#####"
            print(logEntry)
            
            ### add random entries
            print("Individuals Added to System : \n")
            numAdditions = randint(1,3)
           
            ### times for additions
            randTimes = randTime(currentDay + datetime.timedelta(minutes=10),nextDay,numAdditions)
            
            ### add entries
            for i in range(numAdditions):
                timeStamp = randTimes.pop()
                
                #track which names are in database
                newName = self.remainingNames.pop()
                self.inDataBase.append(newName)
                
                #random record for person
                newRecord = Record(convertDateObject(currentDay), convertDateObject(timeStamp), 3)
                
                #add entry to database
                self.addEntry(timeStamp, newName, newRecord,self.DB)

            ### random database events
            print("**********Access Log**********\n")
            numActions = randint(1,10)
            randTimes = randTime(currentDay,nextDay,numActions)
            
            ### generate random events
            for i in range(numActions):
                self.randomAction(deletions, randTimes.pop())
            if self.logging:
                self.logger.newDay()    #add a break in log file for new day
                
            ### increment time interval
            currentDay = nextDay
            nextDay = currentDay + datetime.timedelta(days=1)
        
        ### close log when simulation is done
        if self.logging:
            self.logger.closeLog()
                #choose random action
    
    ### function generates a random event for the database
        # deletions determines if deltion is a valid event
    def randomAction(self, deletions, timeStamp):
        actionSel = random.random() #determines which event is triggered
        if deletions:
            ### entry deletion
            if actionSel >= .95:
                criminal = randPerson(self.inDataBase, remove = True)
                self.deleteEntry(criminal, timeStamp,self.DB)
            ### database search
            elif actionSel < .95 and actionSel >=.1:
                person = randPerson(self.names)
                self.query(person,timeStamp,self.DB)
            ### record update
            else:
                criminal = randPerson(self.inDataBase)
                self.recordUpdate(criminal,timeStamp,self.DB)
        else:
            ### database search
            if actionSel >=.15:
                person = randPerson(self.names)
                self.query(person,timeStamp,self.DB)
            ### record update
            else:
                criminal = randPerson(self.inDataBase)
                self.recordUpdate(criminal,timeStamp,self.DB)
       
    ### add entry to database
    def addEntry(self, time, name, record, dataBase):
        dataBase.put(name,record)
        ### console output
        print("***NEW ENTRY : " + convertDateObject(time))
        print(name + "\n")
        print(record)
        ### add log entry
        if self.logging:
            self.logger.logNewEntry(time,name,record)
    
    ### search for name in database
    def query(self, name, time, dataBase):
        result = ""
        record = dataBase.get(name)
        ###console output
        print("***SEARCH : " + convertDateObject(time))
        # no record found
        if result == None:
            result = name + "\n\n" + "No record exists for this person.\n"
        # record found
        else:
            result = name + "\n\n" + "-- Record -- \n" + str(record)
        print(result)
        ### log entry
        if self.logging:
            self.logger.logSearch(time,name,record)
    
    ### update record
    def recordUpdate(self,name,time,dataBase):
        newOffence = randOffence()
        newOffence = Offence(newOffence, time)
        dataBase.get(name).addOffence(newOffence)
        ### console ouput
        print("***RECORD UPDATE : " + convertDateObject(time))
        result = name + "\n\n"
        result += "New infraction added to recorded : \n" + str(newOffence)
        result += "\n"
        print(result)
        ### log entry
        if self.logging:
            self.logger.logUpdate(time,name,newOffence)
    
    ### delete record from database
    def deleteEntry(self, name, time, dataBase, benchmark = False):
        ### console output
        print("***ENTRY DELETED : " + convertDateObject(time))
        ### incase of benchmarking so that only deletion actions is tested
        if benchmark:
            record = "Testing"
        else:
            record = dataBase.get(name)
        result = name + "\n\n" + "-- Record Removed-- \n" + str(record)
        ### delete record
        dataBase.delete(name)
        print(result)
        ### log entry
        if self.logging:
            self.logger.logDeletion(time,name,record)
            
#    def benchmark(self, operation):
#        self.benchMarkDB = CriminalDataBase()
#        benchmarkNames = shuffle(self.names)
#        namesToAdd = benchmarkNames[:500]
#        operationNames = benchmarkNames[500:]
#        if operation == "insert":
#            pass
#        if operation == "retrieval":
#            pass
#        if operation == "deletion":
#            pass

################################################################################
#
# This class is really just a binary search tree. The only reason it has been 
# inherited here is so that the inorder method can be slighty altered. This
# allows for the entire database to be print with the names and their records
# in alphebetical order.
#
################################################################################

class CriminalDataBase(BinarySearchTree):
    
    def __init__(self):
        BinarySearchTree.__init__(self)
        
    ### in order now prints key and payload
    def _inorder(self,tree):
        if tree != None:
            self._inorder(tree.leftChild)
            print(tree.key)
            print(tree.payload)
            self._inorder(tree.rightChild)

################################################################################
#
# This class repesents a persons criminal record. By default it generates 1-5 
# random offences.
#
################################################################################

class Record():
    
    def __init__(self,startTime = STARTDATE, endTime = ENDDATE, maxOffences = 5):
        self.offenceList = []
        startD = convertDateString(startTime)
        endD = convertDateString(endTime)
        ### generate random offences within the given time interval
        for i in range(randint(1,maxOffences)):
            newOffence = randOffence()
            randTime = randDate(startD,endD)
            self.offenceList.append(Offence(newOffence,randTime))
        #sort list by time
        self.offenceList.sort(key = lambda x: x.getTime())
    
    ### return offence list
    def getRecord(self):
        return self.offenceList
    
    ### format offence list so that it can be output in a single row in the
    ### an event log. Used by eventlogger objects    
    def formatRecord(self):
        result = ""
        for entry in self.offenceList:
            result += str(entry) + "; "
        return result
        
    ### add offence to record
    def addOffence(self, newOffence):       
        self.offenceList.append(newOffence)
        self.offenceList.sort(key = lambda x: x.getTime())
        
    ### converts offence list to a string
    def __str__(self):
        result = ""
        for offence in self.offenceList:
            result += str(offence) + "\n"
        return result
            
################################################################################
#
# This class is an event logger that writes database accesses to a log file in
# csv format.
#
################################################################################            

class EventLogger():
    
    def __init__(self, logPath):
        ###log name
        self.path = logPath
        ### write column headers
        with open(self.path, "w+") as csvfile:
            initalize = csv.writer(csvfile, delimiter = ",")
            initalize.writerow(["TIME","OPERATION","LAST NAME", "FIRST NAME","RESULT"])
        ### log initial set to closed
        self.logOpen = False
        ### used later to write to log
        self.rowWriter = None
        
    ### open log file
    def openLog(self):
        self.rowWriter = open(self.path, "a")
    
    ### close log file
    def closeLog(self):
        if self.rowWriter != None:
            self.rowWriter.flush()
            self.rowWriter.close()
        
    ### write new entry event to log
    def logNewEntry(self, timeStamp, name, record):
        # format record for log
        formattedRecord = record.formatRecord()
        last, first = self.nameSplit(name)
        # write log entry
        self.writeLog("New Entry", timeStamp, last, first, formattedRecord)
        
    ### write search event to log
    def logSearch(self, timeStamp, name, record):
        # if a record is found format it
        if record != None:
            formattedRecord = record.formatRecord()
        # no record found
        else:
            formattedRecord = "No record found."
        last, first = self.nameSplit(name)
        #write log entry
        self.writeLog("Search", timeStamp, last, first, formattedRecord)
    
    ### write record update event  to log
    def logUpdate(self, timeStamp, name, newOffence):
        last, first = self.nameSplit(name)
        #write log entry
        self.writeLog("Record Updated", timeStamp, last, first, newOffence)        
        
    ### write deletion event to log
    def logDeletion(self, timeStamp, name, record):
        # format
        formattedRecord = record.formatRecord()
        last, first = self.nameSplit(name)
        # write to log
        self.writeLog("Record Deleted", timeStamp, last, first, formattedRecord)
    
    ### adds a break in log between days to make it easier to read
    def newDay(self):
        self.writeLog("", "#######", "", "", "")
        
    ### writes a new log entry
    def writeLog(self, entryType, time, lName, fName, result):
        # break in log
        if time != "#######":
            timeStamp = convertDateObject(time)
        # actual entry
        else:
            timeStamp = time
            
        ### allows for editing of log file if log in not currently open
        ### opens and closes log for a single entry
        if self.rowWriter == None:
            with open(self.path, "a") as csvfile:
                addRow = csv.writer(csvfile, delimiter = ",")
                addRow.writerow([timeStamp, entryType, lName, fName, result])
                
        ### writes to log if already open
        else:
            addRow = csv.writer(self.rowWriter, delimiter = ",")
            addRow.writerow([timeStamp, entryType, lName, fName, result])
    
    ### splits a string containing a name into first and last name
    def nameSplit(self, name):
        return name[:name.index(",")], name[name.index(" "):]

################################################################################
#
# This class represents a single criminal offence. It contains the offence and 
# the date on which it occured
#
################################################################################
          
class Offence():
    
    def __init__(self, charge, time):
        self.charge = charge    #offence
        self.time = time
    
    def getTime(self):
        return self.time
    
    #converts offence to single string with offence and date and time
    def __str__(self):
        returnString = convertDateObject(self.time)
        returnString += " -- " + self.charge
        return returnString

#def readNames(filePath):
#    nameList = []
#    with open(filePath, "r") as names:
#        nameReader = csv.reader(names, delimiter = "\t")
#        for line in nameReader:
#            #print(line)
#            nameList.append(line[1] + ", " + line[0])
#    return nameList

### generates a random date and time within the given interval
def randDate(lowerBound, upperBound):
    delta = upperBound - lowerBound
    intDelta = (delta.days * 24 * 60 * 60) + delta.seconds
    ### add a random number of seconds
    randSecs = randrange(intDelta)
    randDate = lowerBound + datetime.timedelta(seconds = randSecs)
    return randDate

### generate a random criminal offence
def randOffence():
    return OFFENCES[randint(0,len(OFFENCES)-1)]
    
### get a random person from a list
def randPerson(nameList, remove = False):
    idx = randint(0,len(nameList)-1)
    
    ### this is used for tracking which names are in a database
    ### pops a random person from the list and returns them
    if remove:
        person = nameList.pop(idx)  # O(n), yetch! Don't know how better to do
                                    # this. Have toyed with the idea of using
                                    # a binary search tree to store names that
                                    # are loaded in database, but I don't know 
                                    # how to randomly access entries in a BST.
    else:
        person = nameList[idx]
    return person

### generates a sorted list of random times and dates
def randTime(startTime,endTime,num):
    times = []
    for i in range(num):
        times.append(randDate(startTime,endTime))
    times.sort()
    times.reverse()
    return times

### converts a datetime object to a string with a date and time
def convertDateObject(date):
    return date.strftime("%Y-%m-%d %H:%M")

### converts a string containing date and time to a datetime object
def convertDateString(date):
    return datetime.datetime.strptime(date, '%Y-%m-%d %H:%M')
  
### insertion benchmark function  
def benchmarkInsert(time, name):
    benchmarkSim.addEntry(time,name,Record(),benchmarkSim.DB)
    
### retrieval benchmark function
def benchmarkGet(time, name):
    benchmarkSim.query(name,time,benchmarkSim.DB)

### deletion benchmark function
def benchmarkDelete(time, name):
    benchmarkSim.deleteEntry(name,time,benchmarkSim.DB,True)
   
### runs a series of benchmarks on a database simulation object 
def benchmarker():
    ### variables that are used by the unholy, hellish, monster, demon that is 
    ### the timeit module, may it it be drawn and quartered, burned, and its
    ### ashes scattered to the seven seas.
    global nameList
    global benchmarkSim
    global benchmarkTime
    global benchmarkPerson
    
    #benchmarkPerson = nameList[0]
    benchmarkTime = randDate(convertDateString(STARTDATE),convertDateString(ENDDATE))
    setrecursionlimit(10000)    # increase recursion depth to allow timeit to
                                # to work its foul, unnatural magic
                                
    #masterList = copy.deepcopy(nameList)
    
    ### timeit's wipping boy
    benchmarkSim = CrimeDBSim(nameList[:1], preLoadPercent = 1)
    
    # holds insertion times
    insertion = []
    
    ### timer for insertion benchmarks
    t = Timer("benchmarkInsert(benchmarkTime,benchmarkPerson)", "from __main__ import benchmarkInsert; from __main__ import benchmarkTime; from __main__ import benchmarkPerson")
    
    ###run insertion benchmarks
    for i in range(50,2000,1):
        benchmarkSim = CrimeDBSim(nameList[:i+1], preLoadPercent = 1)
        #shuffle(nameList)
        benchmarkPerson = randPerson(nameList[i+1:])
        insertion.append(t.timeit(1))
        print(benchmarkSim.DB.length())
    #print(insertion)
    
    # retrieval times
    get = []
    
    ### timer for retrieval benchmarks
    t = Timer("benchmarkGet(benchmarkTime,benchmarkPerson)", "from __main__ import benchmarkGet; from __main__ import benchmarkTime; from __main__ import benchmarkPerson")
    
    benchmarkSim = CrimeDBSim(nameList[:1], preLoadPercent = 1)
    ###run retrieval benchmarks
    for i in range(50,2000,1):
        benchmarkSim = CrimeDBSim(nameList[:i+1], preLoadPercent = 1)
        benchmarkPerson = randPerson(nameList[:i+1])
        get.append(t.timeit(1))
        print(benchmarkSim.DB.length())
    
    # deletion times
    deletion = []
    
    ### timer for  retrieval benchmarks
    t = Timer("benchmarkDelete(benchmarkTime,benchmarkPerson)", "from __main__ import benchmarkDelete; from __main__ import benchmarkTime; from __main__ import benchmarkPerson")
    benchmarkSim = CrimeDBSim(nameList[:1], preLoadPercent = 1)
    ### run deletion benchmarks
    for i in range(2000):
        benchmarkSim = CrimeDBSim(nameList[:i+1], preLoadPercent = 1)
        benchmarkPerson = randPerson(nameList[:i+1])
        deletion.append(t.timeit(1))
        print(benchmarkSim.DB.length())
    #print(get)
    
    
    ###WARNING: I have only been able to get this to work in 2.7, it is untested
    ### with python 3.x, disabled by default
    if plotBenchmarks:
        plt.figure(1)
        p1, = plt.plot(insertion)
        plt.xlabel('Database Size')
        plt.ylabel('Time for 1 insertions')
        plt.title('Insertion Benchmark')
        
        plt.figure(2)
        p2, = plt.plot(get)
        plt.xlabel('Database Size')
        plt.ylabel('Time for 1 retrievals')
        plt.title('Retrieval Benchmark')
        
        plt.figure(3)
        p3, = plt.plot(deletion)
        plt.xlabel('Database Size')
        plt.ylabel('Time for 1 deletion')
        plt.title('Deletion Benchmark')
        plt.show()
        
    
def main():
    ### run benchmarks
    if runBenchmarks:
        benchmarker()
    ### run simulation
    else:
        timeCreated = datetime.datetime.now()
        log1 = EventLogger("simLog-"+ convertDateObject(timeCreated) + ".csv") 
        test = CrimeDBSim(nameList,logger = log1)
        test.runSim()  
    
    print("DONE!")
        
##############################FOR TESTING###############################        
        #crimDataBase = CriminalDataBase()
#    global nameList
#    global benchmarkSim
#    global benchmarkTime
#    global benchmarkPerson
#    benchmarkPerson = nameList[0]
#    benchmarkTime = randDate(convertDateString(STARTDATE),convertDateString(ENDDATE))
    #benchmarkSim = CrimeDBSim(nameList[:500], preLoadPercent = 1)
    #testList = nameList
#    for crim in testList:
#        crimDataBase.put(crim, Record())
#    print(crimDataBase.get("Kennedy, Becky"))
#   
#    log1 = EventLogger("sim1Log.csv") 
#    test = CrimeDBSim(testList,logger = log1)
#    test.runSim()  

    #benchmarker()
    
#    t = Timer("benchmarkInsert()", "from __main__ import benchmarkInsert")
#    print(t.timeit(1))
    
#    test = CrimeBDSim(testList, preLoadPercent = 0)
#    print(test.inDataBase)
    
    #print(test.remainingNames)
   
    #crimDataBase.inorder()
#    test = CrimeBDSim(testList)
#    test2 = Record()
#    currentDay = convertDateString(ENDDATE)
#    nextDay = currentDay + datetime.timedelta(days=1)
##    testOff = Offence(randOffence(), currentDay)
#    crimDataBase.get("Kennedy, Becky").addOffence(testOff)
#    print(crimDataBase.get("Kennedy, Becky"))
#    crimDataBase.inorder()
#    crimDataBase.delete("Kennedy, Becky")
#    print("########################################")
#    crimDataBase.inorder()
#    
    #test.runSim()
    
    #delta = nextDay - currentDay
    #print(delta.minutes)
    #print("Database Log for " + currentDay.strftime("%Y-%m-%d"))
#    print(convertDateObject(currentDay))
#    #print(convertDateObject(nextDay))
#    print(convertDateObject(randDate(currentDay,nextDay)))
################################################################################

if __name__ == "__main__":
    main()












