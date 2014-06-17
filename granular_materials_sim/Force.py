# -*- coding: utf-8 -*-
"""
Created on Sat Mar 02 16:51:28 2013

@author: Nathan Sponberg
"""

from DistMatrix import *
import numpy as ny
from numpy import ma, triu, array, zeros, isnan
from pylab import norm, dot
from IPython.core.debugger import Tracer
debug_here = Tracer()

class Force(object):
    def __init__(self):
        pass

class LennardJonesForce(object):
    def __init__(self, sigma = 1., epsilon = 1.):
        self.sigma = sigma
        self.epsilon = epsilon
        pass

    def __call__(self, container):
        self.c = container
        distCalc = Neighbors()
        distx, disty, distz = distCalc.CalcDist(self.c.xpos,
                                                 self.c.ypos,
                                                 self.c.zpos)
        distx = distCalc.CheckDist(distx, self.c.Lx)
        disty = distCalc.CheckDist(disty, self.c.Ly)
        distz = distCalc.CheckDist(distz, self.c.Lz)
        #debug_here()
        distCalc.UpdateNeighbors(self.c,calcz = False)
        masses = self.c.massVector
        distr = ma.masked_array(sqrt(distx**2 + disty**2 + distz**2),
                                [distx**2 + disty**2 + distz**2 == 0])
        K1 = (ma.divide(self.sigma,distr).filled(0.))**12
        K2 = (ma.divide(self.sigma,distr).filled(0.))**6
        #debug_here()
        KE = sum(sum(triu(array(4.*self.epsilon*(K1-K2)))))
        print KE
        K3 = 2*K1 - K2
        magnitude = 24.*ma.divide(self.epsilon,distr)*K3
        xacl = ny.sum(array((magnitude * ma.divide(distx,distr)).filled(0.)), axis = 1)/masses
        yacl = ny.sum(array((magnitude * ma.divide(disty,distr)).filled(0.)), axis = 1)/masses
        zacl = ny.sum(array((magnitude * ma.divide(distz,distr)).filled(0.)), axis = 1)/masses
        return xacl, yacl, zacl
      
class LennardJonesNeighbors(object):
    def __init__(self, sigma = 1., epsilon = 1.):
        self.sigma = sigma
        self.epsilon = epsilon
        pass    
    
    def __call__(self,container):
        self.c = container
        distCalc = Neighbors()
        xacl = zeros(self.c.numParticles)
        yacl = zeros(self.c.numParticles)
        zacl = zeros(self.c.numParticles)
        masses = self.c.massVector
        print "tick"
        for particle in range(self.c.numParticles):
            neighbors = self.c.neighborList[particle]
            distx, disty, distz, distr = distCalc.NeighborDist(particle, 
                                                               neighbors, 
                                                               self.c)
            K1 = (ma.divide(self.sigma,distr).filled(0.))**12
            K2 = (ma.divide(self.sigma,distr).filled(0.))**6
            K3 = 2*K1 - K2
            magnitude = 24.*ma.divide(self.epsilon,distr)*K3
            #debug_here()
            xacl[particle] = sum(array((magnitude * ma.divide(distx,distr)).filled(0.)))/masses[particle]
            yacl[particle] = sum(array((magnitude * ma.divide(disty,distr)).filled(0.)))/masses[particle]
            zacl[particle] = sum(array((magnitude * ma.divide(distz,distr)).filled(0.)))/masses[particle]
            #debug_here()
        return xacl, yacl, zacl
        
###################################################################
#
# Force object for calculating interactions between granular particles.
# Used for granular materials simulation.
# Notes on granular materials and related formulas can be found here:
# http://wiki.cs.umt.edu/classes/cs477/index.php/Project_II:_Granular_Materials
#
###################################################################
class GranularForces(object):
    def __init__(self, sigma = 1., epsilon = 1., gamma = 5.):
        self.FORCE_BOUND = 500
        self.sigma = sigma
        self.epsilon = epsilon
        self.gamma = gamma
        self.forceTestX = 0
        self.forceTestY = 0
        pass    
    
    def forceCalc(self,distance):
        K1 = (self.sigma/distance)**12
        K2 = (self.sigma/distance)**6
        K3 = 2*K1 - K2
        forceMag = 24*(self.epsilon/distance)*K3
        return forceMag
    
    def wallCalc(self,wall,container,particle):
        distFromWall, xWall, yWall = wall.wallDist(container.xpos[particle],container.ypos[particle])
        wallAclX = 0
        wallAclY = 0
        #debug_here()
        if distFromWall <= (2**(1/6)):
            if (container.ypos[particle] >= wall.endpointsY[0] and container.ypos[particle] <= wall.endpointsY[1]) or wall.isHor:
#                K1 = (self.sigma/distFromWall)**12
#                K2 = (self.sigma/distFromWall)**6
#                K3 = 2*K1 - K2
#                wallMag = 24*(self.epsilon/distFromWall)*K3
                wallMag = self.forceCalc(distFromWall)
                displacement = array([xWall,yWall])
                unitVector = displacement/(displacement**2)
                nans = isnan(unitVector)
                unitVector[nans] = 0.
                dotProduct = dot(displacement, (container.xvel[particle],container.yvel[particle]))
                #debug_here()
                forceVector = -1*self.gamma*dotProduct*unitVector
                wallAclX = forceVector[0]
                wallAclX += wallMag*(xWall/distFromWall)
                wallAclY = forceVector[1]
                wallAclY += wallMag*(yWall/distFromWall)
                      
        #                xacl[particle] += wallAclX
        #             
        #                yacl[particle] += wallAclY
        return wallAclX, wallAclY
    
    def __call__(self, container):
        self.c = container # particle container
        distCalc = Neighbors() 
        #distCalc.UpdateNeighbors(self.c,2**(1/6))
        xacl = zeros(self.c.numParticles)
        yacl = zeros(self.c.numParticles)
        zacl = zeros(self.c.numParticles)
        masses = self.c.massVector
        #print self.c.neighborList[-1]
        for particle in range(self.c.numParticles):
        
            ###Used to determine particle flux through funnel opening for each iteration
            if self.c.ypos[particle] <= self.c.openingPosition and self.c.ypos[particle] >= (self.c.openingPosition - 2):
                self.c.particleFlux[self.c.integrationIteration] += 1
            
            
            neighbors = self.c.neighborList[particle]
            
            ### Calculate distances
            distx, disty, distz, distr, relVelx, relVely, relVelz = distCalc.NeighborDist(particle, 
                                                               neighbors, 
                                                               self.c)                
                                                               
            ### These are the force calculations for particle interactions. This gives us the
            ### gradient along the radial direction between particles, hence distr is used.
            magnitude = self.forceCalc(distr)
            #debug_here()
            #rUnitVector = distr/norm()
            dampingForcex = zeros(len(neighbors))
            dampingForcey = zeros(len(neighbors))
            dampingForcez = zeros(len(neighbors))            
            for i in range(len(neighbors)):
                
                    #print relVelx, relVely
                   #debug_here()
                displacement = array([distx[i],disty[i],distz[i]])
                unitVector = displacement/norm(displacement)
                nans = isnan(unitVector)
                unitVector[nans] = 0.
                dotProduct = dot(displacement, (relVelx[i],relVely[i],relVelz[i]))
                #debug_here()
                forceVector = -1*self.gamma*dotProduct*unitVector
                dampingForcex[i] = forceVector[0]
                dampingForcey[i] = forceVector[1]
                dampingForcez[i] = forceVector[2]
  
            wallAclX = 0
            wallAclY = 0
            for wall in self.c.wallList:
                aclX,aclY = self.wallCalc(wall,self.c,particle)
                if abs(aclX) > self.FORCE_BOUND:
                    aclX = aclX/abs(aclX) * self.FORCE_BOUND
                if abs(aclY) > self.FORCE_BOUND:
                    aclY = aclY/abs(aclY) * self.FORCE_BOUND
                wallAclX += aclX
                wallAclY += aclY
                mag = sqrt(aclX**2 + aclY**2)
                if abs(wallAclX) > self.forceTestX or abs(wallAclY) > self.forceTestY:
                    self.forceTestX = abs(wallAclX)
                    self.forceTestY = abs(wallAclY)
                    print("PING: " + str(self.forceTestX) + ", " + str(self.forceTestY))
                
            accelerationX = sum(array((magnitude * ma.divide(distx,distr)).filled(0.)))/masses[particle] + sum(dampingForcex)/masses[particle] + wallAclX
            accelerationY = sum(array((magnitude * ma.divide(disty,distr)).filled(0.)))/masses[particle] + sum(dampingForcey)/masses[particle] + wallAclY
            
            if abs(accelerationX) > self.FORCE_BOUND:
                accelerationX = accelerationX/abs(accelerationX) * self.FORCE_BOUND
            if abs(accelerationY) > self.FORCE_BOUND:
                accelerationY = accelerationY/abs(accelerationY) * self.FORCE_BOUND
                
            xacl[particle] += accelerationX
            yacl[particle] += accelerationY
            
            zacl[particle] = sum(array((magnitude * ma.divide(distz,distr)).filled(0.)))/masses[particle]
            zacl[particle] += sum(dampingForcex)/masses[particle]
            yacl[particle] += -2.
      
        
        zacl *= 0.
        #print self.c.particleFlux[self.c.integrationIteration]
        return xacl, yacl, zacl
        
        
