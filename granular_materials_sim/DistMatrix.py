
from numpy import size,tile,sqrt,where,array
from numpy import ma
from scipy.spatial.distance import pdist,squareform
from pylab import find
from IPython.core.debugger import Tracer
debug_here = Tracer()

"""
Created Mar 2013
Updated version Thur May 01 15:03:00 2014
Author: Nathan Sponberg
Description:    Distance matrix calulator classes. Has a couple of different
                classes for computing pairwise distance matrices. The Neighboors
                class calculates nearest neighboors and restrictes distance calulations
                to those. This is more effcient when computing things such as 
                molecuar interactions.
"""

class DistMatrix(object):
    def __init__(self):
        pass
    
    ### input: upto three length n lists of position data
    ###        one for each dimension 
    def CalcDist(self, x, y=[], z=[],calcz = False):
        tilex = tile(x , (size(x),1))
        distx = tilex.T - tilex
        tiley = tile(y , (size(y),1))
        disty = tiley.T - tiley
        if calcz:
            tilez = tile(z , (size(z),1))
            distz = tilez.T - tilez
            return distx, disty, distz
        else:
            return distx, disty
        ### output: upto three nXn distance matrices, one
        ###         one for each dim. They are symetric
        ###         with zeros as diagonal entries.
        
    def CalcDistImpv(self, x, y=[], z=[], calcz = False):
        if calcz:
            posMatrix = array([x,y,z])
        else:
            posMatrix = array([x,y])
        posMatrix = posMatrix.T
        distMatrix = pdist(posMatrix, "euclidean")
        distMatrix = squareform(distMatrix)
            
        return distMatrix

    ### Calculate periodic boundary conditions. Allows the calulation of a finite area
    ### for simulation with wrap around borders.
    def UpdatePBoundries(self, vector, domain):     #domain is the size of the boundary area in a single dimension.
        xupdateBoundries = vector
        xupdateBoundries % domain   #now uses modular arithmetic to check
                                    #boundry conditions. More robust than 
                                    #medthod below
        #xupdateBoundries[xupdateBoundries > domain] -= domain #if greater than domain, subtract domain size.
                                                              #sends object from right boundary to left
        #xupdateBoundries[xupdateBoundries < 0] += domain #
        return xupdateBoundries
    
    ### Check pairwise distances to see if distance is shorter across boundries
    ### this assures that forces wrap around boundries as well.
    def CheckDist(self, distMatrix, domain):
        checkDist = distMatrix
        checkDist[checkDist > domain/2.] %= domain
        checkDist[checkDist < -domain/2.] %= domain
#        checkDist[checkDist > domain/2.] -= domain
#        checkDist[checkDist < -domain/2.] += domain
        return checkDist

### DistMatrix object inheritance. Has additional fearutes that allow checking of
### neighboors within a certain radius of a given object. Can be used to compute
### particle interactions only on particles that close to one another.
class Neighbors(DistMatrix):
    def __init__(self):
        #self.neighborList = []
        pass
    
    ### Expects a Container type object as input
    def UpdateNeighbors(self, container, calcz = False, radius = 2.5):
        self.container = container
        self.x = self.container.xpos
        self.y = self.container.ypos
        self.z = self.container.zpos
        self.container.neighborList = []
        self.radius = radius
        
        if calcz:
            distx, disty, distz = self.CalcDist(self.x, self.y, self.z, calcz = calcz)
        else:
            distx, disty = self.CalcDist(self.x, self.y, calcz = calcz)    
        
#        distx = self.CheckDist(distx, self.container.Lx)
#        disty = self.CheckDist(disty, self.container.Ly)
#        distz = self.CheckDist(distz, self.container.Lz)
        
        if calcz:
            distr = sqrt(distx**2 + disty**2 + distz**2)
        else:
            distr = sqrt(distx**2 + disty**2)
     
        for i in range(len(distr)):
            neighborIdx = find(distr[i] <= self.radius)
            neighborIdx.tolist()
            self.container.neighborList.append(neighborIdx)
            
    def UpdateNeighborsImpv(self, container, calcz = False, radius = 2.5):
        self.container = container
        self.x = self.container.xpos
        self.y = self.container.ypos
        self.z = self.container.zpos
        self.container.neighborList = []
        self.radius = radius
        
        distr = self.CalcDistImpv(self.x,self.y,self.z,calcz)
     
        for i in range(len(distr)):
            neighborIdx = find(distr[i] <= self.radius)
            neighborIdx.tolist()
            self.container.neighborList.append(neighborIdx)
            
    def NeighborDist(self, particleIdx, neighborList, container):
        particle = particleIdx        
        self.neighborList = neighborList
        self.c = container
        distx = self.c.xpos[particle] - self.c.xpos[neighborList]
        disty = self.c.ypos[particle] - self.c.ypos[neighborList]
        distz = self.c.zpos[particle] - self.c.zpos[neighborList]
        relVelx = self.c.xvel[particle] - self.c.xvel[neighborList]
        relVely = self.c.yvel[particle] - self.c.yvel[neighborList]
        relVelz = self.c.zvel[particle] - self.c.zvel[neighborList]
        
        #this is always positive, will this create a problem???
        #relVelVector = sqrt(relVelx**2 + relVely**2 + relVelz**2)
        
#        distx = self.CheckDist(distx, self.c.Lx)
#        disty = self.CheckDist(disty, self.c.Ly)
#        distz = self.CheckDist(distz, self.c.Lz)
        
        distr = ma.masked_array(sqrt(distx**2 + disty**2 + distz**2),
                                        [distx**2 + disty**2 + distz**2 == 0])
        return distx, disty, distz, distr, relVelx, relVely, relVelz
    
    def NeighborDistImpv(self, particleIdx, neighborList, container):
        particle = particleIdx        
        self.neighborList = neighborList
        self.c = container
        distx = self.c.xpos[particle] - self.c.xpos[neighborList]
        disty = self.c.ypos[particle] - self.c.ypos[neighborList]
        distz = self.c.zpos[particle] - self.c.zpos[neighborList]
        relVelx = self.c.xvel[particle] - self.c.xvel[neighborList]
        relVely = self.c.yvel[particle] - self.c.yvel[neighborList]
        relVelz = self.c.zvel[particle] - self.c.zvel[neighborList]
        
        #this is always positive, will this create a problem???
        #relVelVector = sqrt(relVelx**2 + relVely**2 + relVelz**2)
        
#        distx = self.CheckDist(distx, self.c.Lx)
#        disty = self.CheckDist(disty, self.c.Ly)
#        distz = self.CheckDist(distz, self.c.Lz)
        
        distr = ma.masked_array(sqrt(distx**2 + disty**2 + distz**2),
                                        [distx**2 + disty**2 + distz**2 == 0])
        return distx, disty, distz, distr, relVelx, relVely, relVelz
    
    def lineDist(self,a,b,c,x,y):
        distFromLine = abs(a*x+b*y+c)/sqrt(a**2+b**2)
        xdist = 0.
        ydist = 0.
        if a != 0:        
            xdist = (a*x + b*y +c)/a
        if b != 0:
            ydist = (a*x + b*y +c)/b        
        return distFromLine, xdist, ydist
##working neighbor loop for **

#for i in range(len(distr)):
#            neighborIdx = where(distr[i] <= self.radius)
#            neighborIdx = neighborIdx[0].tolist()
#            self.container.neighborList.append(neighborIdx)    

#matrix = DistMatrix()
#x1 = array([.5,2.5,4.,2.])
#y1 = array([4.,5.,8.,4.])
#z1 = array([0,0,0,0])
#matrix.UpdatePBoundries(x1,3.)
#matrix.UpdatePBoundries(y1,3.)
#test1,test2, test3 = matrix.CalcDist(x1,y1,z1)


#print test1
#matrix.CheckDist(test1,3.)
#matrix.CheckDist(test2,3.)
#distr = sqrt(test1**2 + test2**2 + test3**2)


#print x1
#print test1
#print test2
#print distr

