# -*- coding: utf-8 -*-
"""
Created on Tue Mar 05 19:27:31 2013

Updated version Thur May 01 15:44:00 2014
Author: Nathan Sponberg
Description:     Main driver class for the granular materials simulation.
                 This is what runs the simulation and the animation.
"""

"""
Things to do:
   * neighborUpdateInterval used for what?
   * data array junk variable, nessacary?
   * count is global, can we fix this?
   * make it so we change particle radius
"""

from Integrate import Verlet
from Force import GranularForces
from Container import InitalizeContainer, Container
from DistMatrix import Neighbors
import matplotlib as plt
from matplotlib import animation
from pylab import * # for plotting commands
from time import time
from scipy import sin,cos,pi
#from numpy import sqrt
from IPython.core.debugger import Tracer
debug_here = Tracer()

##########################
####Initial Conditions####
#END_TIME = 1. #?????????????????????

t = 0. # start time
dt = 0.015 ### Time delta for each integration step
           ### Max at 0.015, speeds up simulation speed, but if time
           ### are too large then to much information is lost.
initConditions = "square_lattice" #used for molecular dynamics
num_frames = 100 #????????????????????????
frameSkip = 3 # number of frames to skip per animation cycle
saveFrame = False #perodically save frame snap shots
neighborUpdateInterval = 1 # check this
data = array([0,0]) #junk variable, let in so as not to break code
count = 0 # frame count 
rad = (2.**(1./6.))/2. # particle radius, not yet variable
Lx = 27./sqrt(3)   # width of container
Ly = 20.    # height of container

floorSize = 18
#wallSize = 3
#slantSize = 10

###Use Radians!!!
slantStart = 20.
slantDegree = pi/3.1
slantMultiplierX = 2*rad*cos(slantDegree)
slantMultiplierY = 2*rad*sin(slantDegree)

#values that represent a wall in the container using the standard
# line equation ax+by+c = 0. Here the syntax of the list should be
#   [a,b,c,(x-coordinate endpoints , y-coordinate endpoints) smallest to largest ]
#wallList = [[-1*sqrt(3),-1,20.,(10/sqrt(3),0,10,20)],[sqrt(3),-1,-7.,(17/sqrt(3),27/sqrt(3),10,20)],[0.,1.,0.,(-20,35.5,0,0)],[sqrt(3),-1,0.,(0,10/sqrt(3),0,10)],[-1*sqrt(3),-1,27,(27/sqrt(3),17/sqrt(3),0,10)]]

wallList = [[-1*sqrt(3),-1,20.,(10/sqrt(3),0,10,20)],[sqrt(3),-1,-7.,(17/sqrt(3),27/sqrt(3),10,20)],[0.,1.,0.,(-20,35.5,0,0)],[1.,0.,-3,(3,3,0,10)],[1,0,-12.5,(12.5,12.5,0,10)]]


gamma = 20

num_rows = 6
#debug_here()
container = Container(floorSize,Lx,Ly)

for wall in wallList:
    container.addWall(wall[0],wall[1],wall[2],wall[3])
    
#degub_here()
    
dist = container.Lx / 5.
vel = dist /5.
useImpvDist = False
animate = True
iterationTimed = 1450
cutOff = 1.*(2*rad)
#########################
#
#for i in range(floorSize):
#    container.addParticle((i*2*(rad) + rad/2, 1.,0.,
#                          0.,0.,0.,
#                          0.,0.,0.),
#                          1.)

#for i in range(wallSize):
#    container.addParticle((rad/2, 1.+(2*rad*(i+1)),0.,
#                          0.,0.,0.,
#                          0.,0.,0.),
#                          1.)
#for i in range(wallSize):
#    container.addParticle((rad/2 + (floorSize-1)*2*rad, 1.+(2*rad*(i+1)),0.,
#                          0.,0.,0.,
#                          0.,0.,0.),
#                          1.)
#
#                        
#for i in range(slantSize):
#    container.addParticle((rad/2 +(i*slantMultiplierX), slantStart-(i*slantMultiplierY),0.,
#                          0.,0.,0.,
#                          0.,0.,0.),
#                          1.)
#for i in range(slantSize):
#    container.addParticle((rad/2 -(i*slantMultiplierX)+(floorSize-6)*2*rad, slantStart-(i*slantMultiplierY),0.,
#                          0.,0.,0.,
#                          0.,0.,0.),
#                          1.)
#    #if i == (slantSize - 1):
#        ####this doesn't work right now
#        #container.openingWidth = container.xpos[-1] - container.xpos[-2]
#        ################
#
container.openingPosition = 8




def set_particles(rows):
  for j in range(rows):
    for i in range(floorSize - (7+j)):
	container.addParticle((i*2*(rad) + (4+j)*rad, slantStart - 2*j*rad,0.,
			      0.,0.,0.,
			      0.,0.,0.),
			      1.)

set_particles(num_rows)
force = GranularForces(gamma=gamma)

#xacl,yacl,zacl = force(container)
#container.xacl = xacl
#container.yacl = yacl
#container.zacl = zacl

integrator = Verlet(dt)

setNeighbors = Neighbors()
if useImpvDist:
    setNeighbors.UpdateNeighborsImpv(container,False, cutOff)
else:
    setNeighbors.UpdateNeighbors(container,False, cutOff)

if animate == True:

    circles = []
    fig = plt.figure(figsize = (10,8))
    ax = plt.gca()
    ax.set_aspect('equal')
    ax.set_xlim((0,container.Lx))
    ax.set_ylim((0,container.Ly))
    for wall in wallList:
        ax.plot([wall[3][0],wall[3][1]],[wall[3][2],wall[3][3]],'b-')
#    ax.plot([0,10/sqrt(3)],[20,10],'b-')
#    ax.plot([17/sqrt(3),27/sqrt(3)],[10,20],'b-')
#    ax.plot([0,10/sqrt(3)],[0,10],'b-')
#    ax.plot([17/sqrt(3),27/sqrt(3)],[10,0],'b-')
    
#    [-1*sqrt(3),-1,15.5,(15.5,17/sqrt(3),0,10)]
    
    def prettify_circle(e):
      color="lightsteelblue"
      facecolor="green"
      alpha=.6
      e.set_clip_box(ax.bbox)
      e.set_edgecolor( color )
      e.set_linewidth(3)
      e.set_facecolor( facecolor )  # "none" not None
      e.set_alpha( alpha )
      return e
    
    for i in range(container.numParticles):
      e = Circle( (container.xpos[i],container.ypos[i]), radius=rad)#, animated = True)
      e = prettify_circle(e)
      circles.append(ax.add_patch(e))
    def init():
      return circles
    
    def next_frame(i): 
        ##integration loop
      #for i in range(frameSkip):
          #integrator(force,container)
          #print container.neighborList
      for j in range(frameSkip):
	  global count
          count += 1
	  if i % 100 == 0:
	    print "frame: {}".format(count)
          if count%neighborUpdateInterval == 0:
              if useImpvDist:
                setNeighbors.UpdateNeighborsImpv(container,False,cutOff)
              else:
                setNeighbors.UpdateNeighbors(container,False,cutOff)
          integrator(force,container)
	  if count % 200 == 0 and saveFrame:
	    title("Frame: {}".format(count))
	    savefig("hourglass_{}.pdf".format(count))
      for i in range(len(circles)):
        circles[i].center = (container.xpos[i], container.ypos[i])
      return circles
    
      #debug_here()
    anim = animation.FuncAnimation(fig,next_frame,init_func=init,
                                   frames=num_frames,interval=1)
#                                   ,blit=True)
    
    plt.show()

else:
    bridge_height_estimate = zeros((iterationTimed,10))
    for i in range(iterationTimed):    
        #if i%neighborUpdateInterval == 0:
        if useImpvDist:
            setNeighbors.UpdateNeighborsImpv(container,False,cutOff)
        else:
            setNeighbors.UpdateNeighbors(container,False,cutOff)
        integrator(force,container)
	xslice_acc = []
	yslice_acc = []
	for h in range(10,20):
	  idx = (container.ypos < h+1)*(container.ypos >= h)
	  yslice_acc.append(average(container.yacl[idx]))
	  xslice_acc.append(average(container.xacl[idx]))
	xslice_acc = array(xslice_acc)
	yslice_acc = array(yslice_acc)
	xslice_acc[isnan(xslice_acc)] = 0
	yslice_acc[isnan(yslice_acc)] = 0
	bridge_height_estimate[i] = yslice_acc 
	if i % 100 == 0 and i > 100:
	  print "frame: {}".format(i)
	  f = figure(figsize=(9,5))
	  subplot(121)
	  plot(range(10,20),xslice_acc)
	  title("Average Hor. Acceleration vs. Height ")
	  xlabel("Height")
	  ylabel("Average X-Acceleration")
	  ylim(-5,15)
	  subplot(122)
	  plot(range(10,20),yslice_acc)
	  title("Average Ver. Acceleration vs. Height ")
	  xlabel("Height")
	  ylabel("Average Y-Acceleration")
	  subplots_adjust(wspace=.4)  # Note this makes space
	  ylim(-5,15)
	  savefig("component_slice_forces_{}.pdf".format(i))

    figure()
    plot(container.particleFlux)
    ylim(-1,5)
    title("Granular Flow Rate") 
    xlabel("Time")
    ylabel("Particle Flux through $y=8$ to $y=10$") 
    savefig("particle_flux.pdf")

    import pickle
    f = open('yaccel.dump','w')
    pickle.dump([bridge_height_estimate,container.particleFlux],f)
    f.close()
# After unpickeling
#In [105]: contourf(arange(1450),arange(10,20),tot_yaccel.T) 
#Out[105]: <matplotlib.contour.QuadContourSet instance at 0xbb2d20c>
#
#In [106]: colorbar() 
#Out[106]: <matplotlib.colorbar.Colorbar instance at 0xbc7058c>
#
#In [107]: title("Avg. Vertical of Force as Various Heights") 
#Out[107]: <matplotlib.text.Text at 0xb741a8c>
#
#In [108]: ylabel("Height") 
#Out[108]: <matplotlib.text.Text at 0xbd2d3cc>
#
#In [109]: xlabel("Time") 
#Out[109]: <matplotlib.text.Text at 0xba5388c>
#
#In [110]: savefig("jam_show.pdf") 

