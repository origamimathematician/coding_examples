#######################################
# Nathan Sponberg
# Ring Fractal
#
# 11/02/13
#
# ringFractal.py
#--------------------------------------
# Draws repeating series of interlocking
# rings. Not really a fractal but still 
# really cool.
#######################################

import turtle
import math

####################Parameters#####################

STARTINGRAD = 180   #starting radius for first ring
RADDEC = 35         #decrement radius by this amount
LAYERS = 2          #number of ring layers (degree)
NUMRINGS = 117        #number of rings at each level


###################################################

def drawRings(aTurtle,startPos,numRings,radius,deg):
    leo = aTurtle
    numR = numRings
    rad = radius
    center = startPos
    deg =deg
    if deg != 0 and rad > 0:
        drawCircle(leo,rad,center)
        newRings = calcCirclePos(numRings,rad)
        for ring in newRings:
            ring = [ring[0]+center[0],ring[1]+center[1]]
            drawRings(leo, ring, numR, rad-RADDEC, deg-1)
        
        
def drawCircle(aTurtle, rad, startPos):
    leo = aTurtle
    leo.speed(0)
    leo.pu()
    leo.goto(startPos)
    leo.setheading(270)
    leo.forward(rad)
    leo.pd()
    leo.setheading(0)
    leo.circle(rad)
    leo.pu()
    leo.goto(startPos)
    
def calcCirclePos(numPoints,rad):
    baseAngle = (2*math.pi)/numPoints
    points = []
    for a in range(1,numPoints+1,1):
        angle = a*baseAngle
        xPos = rad*math.cos(angle)
        yPos = rad*math.sin(angle)
        point = [xPos,yPos]
        points.append(point)
    return points
        
def randomColor():
    """ Generate a color from random RGB components. """
    from random import randint

    # Create a dictionary of random RGB components.
    color = { "r" : randint(0, 255)/255.,
              "g" : randint(0, 255)/255.,
              "b" : randint(0, 255)/255. }

    return color

        
def main():
    leo = turtle.Turtle()
    leo.speed(0)

    # Retrieve the window the turtle will use for drawing.
    turtle.setup(1600,900)
    screen = leo.getscreen()
    screen.reset()
    leo.pu()
    leo.goto(0,0)
    leo.pd()
    
    drawRings(leo,[0,0],NUMRINGS,STARTINGRAD,LAYERS)
    
    leo.pu()
    leo.goto(900,900)
    
    screen.exitonclick()

if __name__ == "__main__":
    main()


        
        
        
