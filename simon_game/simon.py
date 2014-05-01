#######################################
# Nathan Sponberg
# Project 4 - Simon Game
#
# 11/19/13
#
# simon.py
#--------------------------------------
# Play the game Simon. Requires the player
# remember a sequence of colors that increases
# in length as the game progesses.
#######################################

import tkinter
from tkinter import RIGHT, LEFT, TOP, BOTTOM, BOTH, END, RAISED, StringVar
from tkinter.ttk import *
import time
from random import randint
import file_io
import copy

###welcome message
WELCOME = "Welcome to SIMON!\n"
WELCOME+= "This is a game of matching colors.\n"
WELCOME+= "Enter a player name in the text box below and then\n"
WELCOME+= "hit Enter Player Name, then hit Start New Game.\n"
WELCOME+= "A series of colors will appear on screen. You must\n"
WELCOME+= "match then in order. As the game progresses the\n"
WELCOME+= "speed will increase."

SCOREFILE = "HiScores1.csv"

##########################################################################
#
# This class is the main user interface object for the Simon game. It sets
# most of the visual elements of the window as well as button funcationality.
# It has a SimonGame object associated with it that is used to make the logical
# checks and calculations for the game it self.
#
##########################################################################

class SimonUI(Frame):
    
    def __init__(self,parent,displayText,width,height):
        Frame.__init__(self,parent)
        
        self.parent = parent
        self.display = None #display label for displaying colors and messages. initially set to none
        self.playerName = ""    #empty player name, needs user input
        self.displayVar = displayText   #string variable object that will be used to display text on the display label
        self.startUI()  #initialize the UI
    
    ###Set the UI's display, displays initial welcome message and associates the displayVar object
    ###with the UI's display label. This is also where the SimonGame object is instanciated. This is
    ###due to the fact that it needs to now what display object to use.
    def setDisplay(self,display):
        self.display = display
        self.game = SimonGame(self,self.display,self.displayVar)
        self.displayVar.set(WELCOME)
    
    ###Initalizes the UI
    def startUI(self):
        
        self.parent.title("Simon!")
        self.style = Style()
        self.style.theme_use("default")
        
        ###partitions the UI, top part is for the display, bottom for buttons, etc.
        frame = Frame(self, height=0, relief=RAISED, borderwidth=1)
        frame.pack(fill=BOTH, expand=1)
        self.pack(fill=BOTH, expand=1)
        
        ###color button height and width
        BTNW = 10
        BTNH = 5
        
        ######################################################Initalize color buttons#####################################################################
        self.redBtn = tkinter.Button(self, text = "Red",font = ("Helvetica",14),height = BTNH,width = BTNW,command =lambda: self.colorcallback('red'))
        self.redBtn.pack(side=LEFT,padx=5)
        self.greenBtn = tkinter.Button(self, text = "Green",font = ("Helvetica",14),height = BTNH,width = BTNW,command =lambda: self.colorcallback('green'))
        self.greenBtn.pack(side=LEFT,padx=5)
        self.blueBtn = tkinter.Button(self, text = "Blue",font = ("Helvetica",14),height = BTNH,width = BTNW,command =lambda: self.colorcallback('blue'))
        self.blueBtn.pack(side=LEFT,padx=5)
        self.yellowBtn = tkinter.Button(self, text = "Yellow",font = ("Helvetica",14),height = BTNH,width = BTNW,command =lambda: self.colorcallback('yellow'))
        self.yellowBtn.pack(side=LEFT,padx=5)
        
        self.disableColorBtn()  #initially buttons are disabled to prevent unexpected user input
        #################################################################################################################################################
        
        
        ######################################################Initalize  other command buttons###########################################################
        ### exit button
        self.exitBtn = Button(self, text = "Exit",width = 15, command = self.exitcallback)
        self.exitBtn.pack(side = BOTTOM,padx=5,pady=5)
        
        ### start new game
        self.newGameBtn = Button(self, text = "Start New Game",width = 15,command=self.startcallback)
        self.newGameBtn.pack(side = BOTTOM,padx=5,pady=5)
        
        ### enter player name
        self.nameBtn = Button(self, text = "Enter New Name",width=15,command=self.namecallback)
        self.nameBtn.pack(side = BOTTOM,pady=5)
        
        ### entry object for user to input name
        self.nameBox = Entry(self)
        self.nameBox.pack(side = BOTTOM,pady=5)
        
        ### stringvar object for displaying user name on screen
        self.nameVar = StringVar()
        self.nameVar.set("Player Name : ")
        
        ### label to display user name
        self.nameLabel = Label(self,textvariable = self.nameVar)
        self.nameLabel.pack(side = BOTTOM,pady=5)
        #################################################################################################################################################
    
    ### enable all of the color buttons   
    def enableColorBtn(self):
        self.redBtn["state"] = "normal"
        self.greenBtn["state"] = "normal"
        self.blueBtn["state"] = "normal"
        self.yellowBtn["state"] = "normal"
    
    ### disable all color buttons
    def disableColorBtn(self):
        self.redBtn["state"] = "disabled"
        self.greenBtn["state"] = "disabled"
        self.blueBtn["state"] = "disabled"
        self.yellowBtn["state"] = "disabled"
    
    ### enable other buttons
    def enableOtherBtn(self):
        self.newGameBtn["state"] = "enabled"
        self.nameBtn["state"] = "enabled"
    
    ### disable other buttons
    def disableOtherBtn(self):
        self.newGameBtn["state"] = "disabled"
        self.nameBtn["state"] = "disabled"
        
    ### call back function for color button click event 
    def colorcallback(self,color):
        self.display.configure(background=color)  #flash color
        self.game.colorCheck(color) #check to see if chosen color matches next color in the sequence of colors
        self.after(300,lambda: self.display.configure(background = "grey"))
    
    ### call back function for enter name button click event
    def namecallback(self):
        self.playerName = self.nameBox.get()
        self.nameVar.set("Player Name : " + self.playerName)
        self.nameBox.delete(0,END)
    
    ### call back function for start new game button click event
    def startcallback(self):
        if self.playerName == "":
            self.displayVar.set("Enter a player name before\n starting a new game.")
        else:            
            self.displayVar.set("Get Ready...")
            self.disableOtherBtn()
            self.after(3000,lambda: self.game.playRound())
    
    ### call back function for exit button click event
    def exitcallback(self):
        self.parent.destroy()
            
##########################################################################
#
# This class represents a game of Simon. It has all the nessecary methods
# to check for matching player input and for formatting and check hi scores.
#
##########################################################################

class SimonGame():
    
    def __init__(self,parent,display,displayText):
        self.scores = file_io.readScoreCSV(SCOREFILE)   #read in current hi scores, ***WARNING***: DO NOT EDIT THE CSV FILE!!!
                                                        #for some reason if the .csv file is edited and then resaved outside 
                                                        #if this program it will not read in correctly. I have not been able to 
                                                        #figure out why this is. It may have something to do with how Libre Office
                                                        #accesses and saves .csv files. Regardless, the game will correctly read and
                                                        #write to the HiScores file provided it is left alone.
                                                        
        self.formattedScores = self.formatScores(self.scores)   #format the hi scores so they can be printed to display
        self.parent = parent
        self.colorDict = {1:"red",2:"green",3:"blue",4:"yellow"}    #dictionary for accessing colors
        self.colorList = [self.colorDict[randint(1,4)]] #list containing random sequence of colors for the game. stars with a single entry
        self.display = display
        self.displayVar = displayText   #stringvar object for setting display text
        self.SPEED = 1000
        self.colorPos = [0,self.colorList[0]]   #this list tracks the current position that the player has matched upto in the
                                                #color list. Every time the play correctly matches a color this list is set to
                                                #the next color in the list. This is the primary method of determinig if the user's
                                                #input is correct.
        self.score = 0  #player score
        
    ###format raw hi score data (expects a list of ordered pairs, tuples, with a ane and score)
    def formatScores(self,scores):
        formatted = "###HI-SCORES###\n"
        formatted+= "*Name*\t\t*Score*\n"
        place = 1
        for score in scores:
            formatted += str(place) + ". " + score[0] + "\t-\t" + score[1] + "\n"
            place += 1
        #TEST# file_io.writeScoreCSV(scores,"test.csv")
        return formatted
    
    ###check to see if current player score is a new hi score
    def checkScores(self,score):
        pos = 0 #position of score currently being compared
        newHi = False   #new hi score?
        ###check current score against all recorded hi scores
        for currentScore in self.scores:
            if int(currentScore[1]) <= score and newHi == False:    #Check to see if the current score is higher
                newHi = True
                pos = self.scores.index(currentScore)
        if newHi == True:   #if new hi score then replace old hi score
            newScore = (str(self.parent.playerName),str(score))
            #print(newScore)
            self.scores[pos] = newScore
            
        return newHi, self.scores[pos], pos+1   #return new hiscore if thhere is one and its position in list
        
    ###add an additional random color on to the color list
    def addColor(self):
        self.colorList.append(self.colorDict[randint(1,4)])
    
    ###play a round of Simon, cycles through colors on display
    def playRound(self):
        #self.parent.disableOtherBtn()
        self.displayVar.set("Get ready...GO!")
        self.cycleColors(self.colorList)
    
    ###pepares the user to play another round of Simon
    def nextRound(self):
        self.parent.disableColorBtn()
        #self.parent.disableOtherBtn()
        self.displayVar.set("Get Ready...")
        self.parent.after(2000,lambda: self.playRound())
    
    ###reset the game after the play has failed to match a color, also checks for new hi score
    def reset(self):
        newHi, hiScore, scorePos = self.checkScores(self.score) #check for hi score1
        if newHi:   #new high score found
            self.formattedScores = self.formatScores(self.scores)
            #print(self.formattedScores)
            ###special output telling the user that they obtain new hi score
            displayNewHi = self.formattedScores
            displayNewHi += "You got a new HI SCORE!\n"
            displayNewHi += "New hi score : " + str(scorePos) + ". " + hiScore[0] + " - " + hiScore[1]
            self.displayVar.set(displayNewHi)
            #rewrite old hi score file
            file_io.writeScoreCSV(self.scores,SCOREFILE)
        else:   #no new hi score
            self.displayVar.set(self.formattedScores)
            
        ###Block all user input for a few seconds so as to display GAME OVER message
        self.parent.disableColorBtn()
        self.parent.enableOtherBtn()
        
        ###reset player name
        self.parent.playerName = ""
        self.parent.nameVar.set("Player Name : ")
        
        ###reset color list and position
        self.colorList = [self.colorDict[randint(1,4)]]
        self.resetColorPos()
        
        ###reset speed and score
        self.SPEED = 1000
        self.score = 0
        
        self.parent.enableOtherBtn()
        
    ###reset color position variable
    def resetColorPos(self):
        self.colorPos = [0,self.colorList[0]]
    
    ###check to see if a color input by user matches next color in color list
    def colorCheck(self,color):
        match = False
        
        ###match found
        if color == self.colorPos[1]:
            match = True
        
        ###check for match and to see if user has completed current round
        if match and self.colorPos[0] == len(self.colorList)-1: #round is complete
            self.displayVar.set("Colors matched, round complete.")
            self.parent.disableColorBtn()   #block user input
            self.score = len(self.colorList)    #update score
            ###increase speed for every four rounds completed, speed increased by 25% of current speed each time
            if self.score % 4 == 0:
                self.SPEED = int(self.SPEED*.75)
            self.addColor() #add new color
            self.resetColorPos()
            self.parent.after(2000,self.nextRound)  #start next round
        ###color matched but round not completed yet
        elif match:
            ###step color position up by one
            self.colorPos[1] = self.colorList[self.colorPos[0]+1]
            self.colorPos[0] = self.colorPos[0] + 1
        ###mismatched color
        else:
            self.displayVar.set("Mismatched color\n GAME OVER\n\nRounds Completed : " + str(self.score))
            self.parent.disableOtherBtn()
            self.parent.disableColorBtn()
            self.parent.after(2000,self.reset)
        #print(self.colorPos)

    ###flash the colors in the color list on screen. Used to prompt user to match the colors
    def cycleColors(self,colors):
        ###recursively cycles through colors
        if len(colors) > 0:
            #switch from grey
            if self.display["background"] == "grey":
                self.setBG(colors[0])
                self.parent.after(self.SPEED,lambda: self.cycleColors(colors[1:]))
            #swtich to grey
            else:
                self.setBG("grey")
                self.parent.after(self.SPEED,lambda: self.cycleColors(colors))
        ###all colors have cycled, reset screen to grey and enable color buttons
        elif len(colors) == 0:
            self.setBG("grey")
            self.parent.enableColorBtn()
            self.displayVar.set("Match the colors.")
            #print(self.colorPos)
    
    ###set background color of display label
    def setBG(self,color):
        self.display.configure(background = color)
        
#####################################
#
# main fucntion, sets up Simon game and
# starts tkinter main loop for the GUI
#
#####################################

def main():
    #set main GUI window
    root = tkinter.Tk()
    root.geometry("800x600+300+100")
    W,H = 800,435
    
    #stringvar object for displaying text on screen
    displayVar = StringVar()
    #displayless SimonUI object
    interface = SimonUI(root,displayVar,W,H)
    
    #create display for SimonUI
    colorDisplay = tkinter.Label(root,textvariable = displayVar, font = ("Helvetica",16))
    colorDisplay.place(x=0,y=0,width = W, height = H)
    colorDisplay.configure(background="grey")
    
    #pass display to the SimonUI
    interface.setDisplay(colorDisplay)
    
    #start tkinter mainloop
    root.mainloop()

if __name__ == "__main__":
    main()




    
