#
#
# Shadow3 python script to create a tolerances study of a mirror
#
#
# It uses the currently defined Shadow system (files start.00 and start.01)
#
# It scans a list of variables, e.g., the mirror rotations X_ROT,Y_ROT,Z_ROT
#
# Output: is the H and V fwhm to be written in a file (tolerances.spec)
#
#
# Author: Manuel Sanchew del Rio
#         ESRF (c)
#         2012-03-08
#
#

#
# import block
#

import numpy 
#import sys
import copy

import Shadow as sh
import Shadow.ShadowTools as st


#
# initialize Shadow containers
#

src = sh.Source()
oe1 = sh.OE()
beamSource = sh.Beam()

#
# >>>>>>>>>>>  inputs <<<<<<<<<<<<<
#

#
#flags
#
source_create = 1  # create source (1) or read from file (0)
noplot = 1         # 0=plot   1=no plot
write  = 0         # 0=No     1=Yes (write shadow binary files)

#
#scanning: 
#
scan1 = numpy.linspace(-0.01,0.01,31) # from, to, npoints

# scan a single variable: 
#scanWhat  = 'X_ROT'
#scanIndex=0
# or scan a list of variables:
scanList=['X_ROT','Y_ROT','Z_ROT']

#
# >>>>>>>>>>>  calculations <<<<<<<<<<<<<
#

#
# open output file
#
f = open('tolerances.spec', 'wb')
header="#F tolerances.py\n"
f.write(header)

#
# get Shadow source 
#
if source_create == 1:
    # creates source from start.00 if "-s" 
    src.load('start.00')
    beamSource.genSource(src)
    if write:
        beamSource.write("begin.dat")
else:
    # reads source from binary file" start.00
    beamSource.load('begin.dat')  



#
# get fwhm of the source
#
g1 = st.plotxy(beamSource,1,3,calfwhm=1,nbins=200,noplot=noplot)
fw0 = numpy.array([g1.fwhmx,g1.fwhmy])

#
# start loop on scanList
#
for scanIndex,scanWhat in enumerate(scanList):
    # loop on scanning variable
    out   = numpy.zeros( (9,scan1.size) ) 
    
    for i in range(scan1.size):
        oe1.load("start.01")
        #run system with only 1 oe
        if write:
            oe1.FWRITE=0 # write(0) or not(3) binary files
        else:
            oe1.FWRITE=3 # write(0) or not(3) binary files
        oe1.F_MOVE=1
        #oe1.X_ROT=scan1[i]
        setattr(oe1,scanWhat,scan1[i])
        beam       = sh.Beam()
        beam.rays  = copy.deepcopy(beamSource.rays)
        beam.traceOE(oe1,1)
        #analysis of the final spot
        #g1 = st.plotxy(beam,1,3,calfwhm=2,nbins=200,noplot=noplot)
        g1 = st.plotxy(beam,1,3,calfwhm=2,nbins=500,noplot=noplot,nolost=1)
        fw1 = numpy.array([g1.fwhmx,g1.fwhmy])
        print "------------------------------------"
        print "fwhm source: ",fw0
        print "fwhm spot: ",fw1
        print "demagnification: ",fw0/fw1
        print "------------------------------------"
        # store results
        out[0,i] = scan1[i]
        out[3,i] = fw1[0]*1e4
        out[4,i] = fw1[1]*1e4
        out[7,i] = g1.intensity
        out[8,i] = g1.intensityinslit
    
    labels="#L  "+ \
        scanWhat+" deg  "+ \
        scanWhat+" arcsec  "+ \
        scanWhat+" urad  fwhm_h [um]  fwhm_v [um]  H[%]  V[%]  intens  intensInFWHM"
    out[1,:] = out[0,:]*3600.0              # arcsec
    out[2,:] = out[0,:]*numpy.pi/180.0*1e6  # microrad
    out[5,:] = out[3,:]/out[3,out.shape[1]/2]*100.0
    out[6,:] = out[4,:]/out[4,out.shape[1]/2]*100.0
    
    
    
    #
    # >>>>>>>>>>>> outputs <<<<<<<<<<<<<<<<
    #
    
    #write spec formatted file
    
    tmp = range(out.shape[0])
    tmp =  (str( tmp  ).strip('[]')).split()
    
    #labels="#L"
    #for i in range(out.shape[0]): 
    #  labels=labels+"  col"+tmp[i]
    
    header="\n#S "+str(scanIndex+1)+" "+scanWhat+"\n#N "+ \
        str(out.shape[0])+"\n"+labels+"\n"
    f.write(header)
    
    for i in range(out.shape[1]):
       f.write( ("%20.11e "*out.shape[0]+"\n") % tuple( out[:,i].tolist())  )
       print( ("%20.11e "*out.shape[0]+"\n") % tuple( out[:,i].tolist())  )
    
    
# terminate
f.close()
print "File written to disk: tolerances.spec"

#st.plt.close("all")
