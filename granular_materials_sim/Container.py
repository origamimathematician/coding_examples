# -*- coding: utf-8 -*-
"""
Created on Sat Mar 02 15:14:08 2013
Updated version Thur May 01 15:03:00 2014
Author: Nathan Sponberg
Description:     a container class that holds a number particles that interact
                 with one another. 
                 Notes on molecular interaction and formulas can be found here:
                 http://wiki.cs.umt.edu/classes/cs477/index.php/Molecular_Dynamics_Notes
                 
                 Notes on granular materials can be found here:
                 http://wiki.cs.umt.edu/classes/cs477/index.php/Project_II:_Granular_Materials       
        
            Functions:
            

"""


#Things to do:
#    *Resolve how masses are implemented and used
#    *Adjust how inital conditions are track when adding particles
#    *Variables from line 77-84, still used?

import csv
from numpy import array, hstack, size, tile, ones, sqrt

class Wall(object):
    def __init__(self,a,b,c,endpoints):
        self.a = a
        self.b = b
        self.c = c
        self.endpointsX = endpoints[0:2]
        self.endpointsY = endpoints[2:4]
    
    def wallDist(self,x,y):
        distFromWall = abs(self.a*x+self.b*y+self.c)/ (sqrt(self.a**2+self.b**2))
        xdist = 0.
        ydist = 0.
        if self.a != 0:        
            xdist = (self.a*x + self.b*y +self.c)/self.a
        if self.b != 0:
            ydist = (self.a*x + self.b*y +self.c)/self.b        
        return distFromWall, xdist, ydist

class Container(object):
    def __init__(self, floorSize, xdim, ydim, zdim = 0, springEqDist = 2.*2.**(1./6.)):
        self.floorSize = floorSize
        #self.wallSize = wallSize
        #self.slantSize = slantSize
        self.Lx = xdim
        self.Ly = ydim
        self.Lz = zdim #set to zero by default for 2-D sims
        self.wallList = []  #stores all wall objects in the simulation area
        
        #equilibrium distance between lattice particles, used for "spring"
        #forces in partical lattice (molecular friction)
        self.springEqDist = springEqDist 

        #initial position of right most lattice particle
        # used for molecular friction sim. Not used for this simulation
        self.initDragPos = 0. 
        
        ###postions, list of coordinates for particles in the system.
        ### First values in each list represent coordinates of first particle etc.
        self.xpos = array([],dtype='float64')
        self.ypos = array([],dtype='float64')
        self.zpos = array([],dtype='float64')

        ###velocities, similar to above
        self.xvel = array([],dtype='float64')
        self.yvel = array([],dtype='float64')
        self.zvel = array([],dtype='float64')

        ###accelerations, this is needed for certain integration algorithms, lots of stuff
        ### to track :P
        self.xacl = array([],dtype='float64')
        self.yacl = array([],dtype='float64')
        self.zacl = array([],dtype='float64')
        
        ###initial system conditions, used to reset container for multiple simulations
        
        ### Do I need this???????????????????????
        self.xposinit = array([],dtype='float64')
        self.yposinit = array([],dtype='float64')
        self.zposinit = array([],dtype='float64')
        self.xvelinit = array([],dtype='float64')
        self.yvelinit = array([],dtype='float64')
        self.zvelinit = array([],dtype='float64')
        self.xaclinit = array([],dtype='float64')
        self.yaclinit = array([],dtype='float64')
        self.zaclinit = array([],dtype='float64')

        ###masses
        
        ###Is this still used?????????????????????
        self.massVector = array([],dtype='float64')
        self.numParticles = 0

        ###empty variable used later
        ###Is this still used?????????????????????
        self.adjMatrix = array([],dtype='bool')
        self.dragForce = []
        self.maxDragForces = []
        ###
        self.openingWidth = 0.
        self.openingPosition = 0.
        self.particleFlux = list()
        self.integrationIteration = 0

    def addParticle(self,initState,mass): #add particles to the system
        self.numParticles += 1
        self.xpos = hstack((self.xpos, initState[0]))
        self.ypos = hstack((self.ypos, initState[1]))
        self.zpos = hstack((self.zpos, initState[2]))
        self.xvel = hstack((self.xvel, initState[3]))
        self.yvel = hstack((self.yvel, initState[4]))
        self.zvel = hstack((self.zvel, initState[5]))
        self.xacl = hstack((self.xacl, initState[6]))
        self.yacl = hstack((self.yacl, initState[7]))
        self.zacl = hstack((self.zacl, initState[8]))
        
        ###Setting initial state like this could cause problems in the furture if particles are added after 
        ### simulation is started. Should be fixed at some point, but does not 
        ### break program at this point
        self.xposinit = self.xpos
        self.yposinit = self.ypos
        self.zposinit = self.zpos
        self.xvelinit = self.xvel
        self.yvelinit = self.yvel
        self.zvelinit = self.zvel
        self.xaclinit = self.xacl
        self.yaclinit = self.yacl
        self.zaclinit = self.zacl

        self.massVector = hstack((self.massVector, mass))
        
    def addWall(self,a,b,c,endpoints):
        newWall = Wall(a,b,c,endpoints)
        self.wallList.append(newWall)

    def getMassMatrix(self): ###not used any more, possibly useful in the future
        massMatrix = tile(self.massVector, (size(self.massVector),1))
        massMatrix = massMatrix.T
        return massMatrix

    ###sets adjacency martix for particle lattice, used to compute which particles
    ###have stiff springs connecting them. Used for molecular friction simulation.
    def setAdjacency(self): 
        self.initDragPos = self.xpos[-1]
        self.adjMatrix = ones((self.numParticles,self.numParticles), dtype = bool)
        
        ###the order in which lattice particles are added (see driver class) means that for a given
        ###paticle in the lattice is connected by springs to the two particles that
        ###are before it and after it in the index list. This is always the case except
        ###for the last two and the first two particles.
        for i in range(self.floorSize, self.numParticles):
            #first particle in lattice no particles before it in the index
            if i == self.floorSize:
                ##adds springs between this particle and the next two particles
                ##other end particles are similar to this
                self.adjMatrix[i,[i+1,i+2]] = False
            #second particle in lattice
            elif i == self.floorSize + 1:
                self.adjMatrix[i,[i+1,i+2]] = False
                self.adjMatrix[i,i-1] = False
            #second to last particle in lattice
            elif i == self.numParticles-2:
                self.adjMatrix[i,i+1] = False
                self.adjMatrix[i,[i-1,i-2]] = False
            #last particle in lattice
            elif i == self.numParticles-1:
                self.adjMatrix[i,[i-1,i-2]] = False
            #middle particles in lattice
            else:
                self.adjMatrix[i,[i+1,i+2]] = False
                self.adjMatrix[i,[i-1,i-2]] = False
        return self.adjMatrix


    def summary(self):
        print self.xpos
        print self.ypos
        print self.zpos
        print self.xvel
        print self.yvel
        print self.zvel
        print self.xacl
        print self.yacl
        print self.zacl
        print self.massVector
        print self.numParticles
        print self.Lx
        print self.Ly
        print self.Lz

    def posSummary(self):
        print self.xpos
        print self.ypos
        print self.zpos
        
    ##reset container to initial conditions of all particles, useful for multiple
    ##simulations on one container
    def reset(self):
        self.xpos = self.xposinit
        self.ypos = self.yposinit
        self.zpos = self.zposinit
        self.xvel = self.xvelinit
        self.yvel = self.yvelinit
        self.zvel = self.zvelinit
        self.xacl = self.xaclinit
        self.yacl = self.yaclinit
        self.zacl = self.zaclinit
        self.dragForce = []
    
#######Initialize container from a file#########
def InitalizeContainer(self,filename):
    with open(filename, 'r') as f:
      reader = csv.reader(f,delimiter=" ")
      self.numParticles = int(reader.next()[0])
      xdim = float(reader.next()[0])
      ydim = float(reader.next()[0])
      zdim = float(reader.next()[0])
      container = Container(xdim,ydim,zdim)
      for row in reader:
          state = [float(i) for i in row]
          container.addParticle(state[0:-1],state[-1])
      return container

def main():
    testWall = Wall(1,2,3,[3,4,5,6])
    print(testWall.wallDist(3,6))
    
if __name__ == "__main__":
    main()
