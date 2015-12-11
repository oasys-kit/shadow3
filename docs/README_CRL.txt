--------------------------------------------------------------------------------

                      CRLs and TRANSFOCATORS in SHADOW

--------------------------------------------------------------------------------
     
              version 1: 2012-01-27  srio@esrf.eu
              version 2: 2012-05-16  srio@esrf.eu
              version 3: 2013-04-11  srio@esrf.eu  transfocator added
              version 4: 2015-07-10  srio@esrf.eu  added command lines and 
                                                   python scripts
--------------------------------------------------------------------------------

Contents:

1 Geometry of a CRL.
2 Example: Simulating the CRL of Snigirev et al. Nature 384, p49 (1996)
3 Transfocators
4 Python

--------------------------------------------------------------------------------

1) Geometry of a CRL.
---------------------
In SHADOW ONE lens is defined by TWO interfaces. 

A CRL (Compound refractive lens) is a stack of N lenses or 2N interfaces.

This graph shows a CRL of TWO lenses or FOUR interfaces:

                                                                          
                  *****************    ******************                 
                  *****************    ******************                 
                  *****************    ******************                 
                   ***************      *****************                 
                 *  **************       *************** *                
               **    ***********          *************   **              
             **       *********            ***********      **            
          ***          *******              ********          ***         
        **              *****                ******              **       
      **                 ***                  ****                 **     
    **                   ***                  ****                   **   
  **                     ***                  ****                   **   
    **                  *****                 *****                **     
      ***              *******                *****              **       
         ***          *********              *******          ***         
Source      **       ***********           **********       **    Image
              ***   **************        ************    **              
                 * ****************     ******************                
                  *****************    ******************                 
                  *****************    ******************                 
                  *****************    ******************                 
                  *****************    ******************                 
                  *****************    ******************           
                        <-->
                        ddIn
                            <--- ddV ----->
<-------p0------------>                             <----- q0 ------->

The parameters that will be used to define the CRL are: 

 p0: physical distance from source plane (or continuation plane) to CRL's first interface 
 q0: physical distance from last interface of the CRL to the image plane (or continuation plane)
 ddIn: lens thickness [along optical axis, in material] 
 ddV:  lens length [along optical axis, in vacuum] 

In addition, one may define the focal distances (needed to compute the interface shape and parameters): 

 pp: focal source-crl distance     
 qq: focal crl-image distance    


2) Example: Simulating the CRL of Snigirev et al. Nature 384, p49 (1996)
------------------------------------------------------------------------

This is an example to run a CRL in SHADOW. 

Note: This example shows an optical system that contains only one CRL.
      For the moment, it is not easy to insert a CRL in between other oe's.

data: 
    source: 
       E = 14 keV (single line)
       Real space Gaussian: FWHM = 150 um or Sigma = 63.8 um
       Divergence space Conical: angle=10e-6 rad
 
    Lens: 
       N=30 holes, therefore 60 interfaces
       R=300 um
       ddIn = 25 um
       ddV = 2 R = 600 um
       Aluminium: n=1-delta=0.99999720 ; delta = 2.8e-6; mu=28cm^-1 (density=2.7 g/cm3)

    Distances:
       F = R/2/N/delta = 178.57 cm
          To obtain this F, we use (1/F=1/pp+1/qq):
             pp = 3000 cm
             qq = 189.87
    

STEP 1: Run the preprocessor (precrl) to create the SHADOW input file crl.01
        You can answer the questions manually (after entering "shadow3 prerefl")

STEP 2: Run SHADOW (runcrl)

        shadow3> runcrl
        File with source (default: begin.dat): begin.dat
        File with CRL definition (default: crl.01): crl.01
        File final image (star.xx type, default: final.01): final.01
        ... 

These steps can be run doing the following commands:

echo -e "FDISTR =  5\nSIGMAX = 63.8e-4\nSIGMAY = 0.001\nCONE_MAX = 1e-5\nCONE_MIN = 0.0\nFSOUR =  3\nF_COLOR = 1\nPH1 = 14000.0"> start.00
sed -n '/^#START crl_snigirev1996.inp/,/^#END crl_snigirev1996.inp/p' README_CRL.txt > crl_snigirev1996.inp
shadow3 < crl_snigirev1996.inp 

where crl_snigirev1996.inp contains: 
#START crl_snigirev1996.inp
source
systemfile

precrl
0
1
1
0
60
0
1.0000000
0.9999972
0.0
28.0
3000
189.87342
1
300e-4
0
25e-4
600e-4
0
crl.01


runcrl
begin.dat
crl.01
final.01

exit

#END crl_snigirev1996.inp


Notes: 

The result file is final.01. wich gives a Z profile of 10.3 um
This result includes absorption
This runs do not write intermediate SHADOW files (start.xx star.xx mirr.xx)
To do so, select Verbose-Shadow-Run? to 3.


It is possible to run parabolic or elliptical lenses instead of spherical. 
Use the corresponding Lens interface shape: 
 1 - Sphere
 2 - Ellipsoid
 4 - Paraboloid
 5 - Plane
 7 - Hyperboloid

3) Transfocators
----------------

A transfocator is made of a stack of blocks, each block is a CRL that may contain a different 
number of lenses (by adding or substrcting lenses). To run SHADOW for transfocator, the "precrl"
preprocessor can accept an input file with an easy condensed syntax. 

For creating the input file, it is suggested first to create a template by doing: 

 shadow3> precrl
 PRECRL: Create a stack of lenses: 
 0 - single CRL
 1 - transfocator (stack of CRLs)
 ?>  1
  
 The transfocator stack of CRLs must be defined in "transfocator definition" file: 
 0 - Create a "transfocator definition" template file
 1 - Load a "transfocator definition" input file
 ?>  0
 File name to dump template (default: mytransfocator.dat): 
  "transfocator definition" file written to disk: mytransfocator.dat
  modify it and re-run "precrl"


The created file mytransfocator.dat contains a header with the documentation. 

#  Geometry of a single CRL within a transfocator
#                     .                  .                                 
#              |      .*********+++++++++.       |          
#              |      . *******  +++++++ .       |            
#              |      .  *****    +++++  .       |            
#              |      .   ***      +++   .       |            
#              |      .   ***      +++   .       |            
#              |      .   ***      +++   .       |            
#              |      .  *****    +++++  .       |            
#              |      . *******  +++++++ .       |           
#              |      .*********+++++++++.       |            
#                     .   <->            .
#                     . crl_interThickness
#                     .<-------><------->.
#                     .          crl_thickness
#              |<---->.                  .<----->|
#       crl_fs_before .                  .crl_fs_after
#
#

NOTES ON GEOMETRY: 

   - The crl_thickness of the single-lens in transfocator corresponds to ddIn+ddV in CRL's geometry.
   - crl_interThickness in trasfocator is equal to ddIn in CRLs.
   - The crl_fs_before in transfocator corresponds to p0+0.5*ddV n CRLs (and crl_fs_after=q0+0.5*qq).
   - The inputs for the transfocator have less flexibility than the CRL, in the sense that 
     only external definition of the lens (radius, sizes) is possible, and the only way to 
     define the refraction is using the preprocessor prerefl.
     
Example: the following line creates the input for Snigirev's lens discussd in previous section: 

# Data for CRL of Snigirev et al. Nature 384, p49 (1996)
30  4 1 300e-4  -1.0 600e-4 25e-4 625e-4  3000  189.87 Al5_55.dat


4) Python
---------

A set of routines have been written in python to define lenses, CRLs and 
transfocators. See ShadowLibExtensions.py file for code and examples. The 
Syntax is the following: 

lens = Shadow.CompoundOE()
lens.append_lens(...)

crl = Shadow.CompoundOE()
crl.append_crl(...)

tf = Shadow.CompoundOE()
tf.append_transfocator(...)

The following commads will create and run the Snigirev example using the 
CRL and the TRANSFOCATOR mode: 


sed -n '/^#START crl_snigirev1996.py/,/^#END crl_snigirev1996.py/p' README_CRL.txt > crl_snigirev1996.py
python3 crl_snigirev1996.py


sed -n '/^#START transfocator_snigirev1996.py/,/^#END transfocator_snigirev1996.py/p' README_CRL.txt > transfocator_snigirev1996.py
python3 transfocator_snigirev1996.py

where the python scripts are: 

#START crl_snigirev1996.py

#
# This is the CRL in Snigirev et al. Nature (1996)
# 
#
#
# For more information, see the file README_CRL.txt and documentation inside
# ShadoeLibExtensions.py in the shadow3 distribution.
#
# 

#import os
import Shadow


iwrite = 0

#os.system("/bin/rm -r begin.dat star.* start.* end.* mirr.*")

#
# Source: Gaussian in real space, Conical in divergence space, Monochromatoic at 24keV
#
src = Shadow.Source()
src.set_energy_monochromatic(14000.0)
src.set_spatial_gauss(63.8e-4,63.8e-4)
src.FDISTR = 5
src.CONE_MAX = 1e-5
src.CONE_MIN = 0.0
if iwrite: src.write("start.00")

beam = Shadow.Beam()

beam.genSource(src)
if iwrite: 
    beam.write("begin.dat")
    src.write("end.00")

#
# CRL 
#

# This graph shows a CRL of TWO lenses 
# 
#                                                                           
#                   *****************+++++++++++++++++                 
#                   *****************+++++++++++++++++                 
#                   *****************+++++++++++++++++                 
#                    *************** +++++++++++++++++                 
#                  *  **************  +++++++++++++++ *                
#                **    ***********     +++++++++++++   **              
#              **       *********       +++++++++++      **            
#           ***          *******         ++++++++          ***         
#         **              *****           ++++++              **       
#       **                 ***             ++++                 **     
#     **                   ***             ++++                   **   
#   **                     ***             ++++                   **   
#     **                  *****            +++++                **     
#       ***              *******           +++++              **       
#          ***          *********         +++++++          ***         
# Source      **       ***********      ++++++++++       **    Image
#               ***   **************   ++++++++++++    **              
#                  * ****************++++++++++++++++**                
#                   *****************+++++++++++++++++                 
#                   *****************+++++++++++++++++                 
#                   *****************+++++++++++++++++                 
#                   *****************+++++++++++++++++                 
#                   *****************+++++++++++++++++           
#                         <-->
#                         interthickness
#                   <-- thickness -->
# <-------p0------------>                             <----- q0 --->
# 


crl = Shadow.CompoundOE(name = 'crl_snigirev1996')
p0 = 3000.0
q0 = 189.87
crl_shape =  1 #  1=Sphere, 2=Ellipsoid, 4=Paraboloid, 5=Plane, 7=Hyperboloid

crl.append_crl(p0, q0, nlenses=30, slots_empty=0, radius=300e-4, thickness=600e-4, interthickness=25e-4,
    surface_shape=crl_shape, convex_to_the_beam=0, diameter=600e-4, cylinder_angle=0.0,
    prerefl_file=None, refraction_index=0.99999720, attenuation_coefficient=28.0, \
    use_ccc=0)

beam.traceCompoundOE(crl,write_start_files=iwrite,write_end_files=iwrite,write_star_files=iwrite)

if iwrite: beam.write("final.01")

Shadow.ShadowTools.plotxy(beam,1,3,nolost=1,nbins=151,yrange=[-0.01,0.01])
#END crl_snigirev1996.py

#START transfocator_snigirev1996.py

#
# This is the CRL in Snigirev et al. Nature (1996) defined as TRANSFOCATOR
# (the 30 lenses have been divided in two CRL blocks of 20+10) 
# 
#
# For more information, see the file README_CRL.txt and documentation inside
# ShadoeLibExtensions.py in the shadow3 distribution.
# 

#import os
import Shadow


iwrite = 0

#os.system("/bin/rm -r begin.dat star.* start.* end.* mirr.*")

#
# Source: Gaussian in real space, Conical in divergence space, Monochromatoic at 24keV
#
src = Shadow.Source()
src.set_energy_monochromatic(14000.0)
src.set_spatial_gauss(63.8e-4,63.8e-4)
src.FDISTR = 5
src.CONE_MAX = 1e-5
src.CONE_MIN = 0.0
if iwrite: src.write("start.00")

beam = Shadow.Beam()

beam.genSource(src)
if iwrite: 
    beam.write("begin.dat")
    src.write("end.00")

#
# TRANSFOCATOR
#

#  Geometry of a single CRL within a transfocator
#
#              |<----------i-th CRL ------------>|
#              |      .                  .       |                         
#              |      .*********+++++++++.       |          
#              |      . *******  +++++++ .       |            
#              |      .  *****    +++++  .       |            
#              |      .   ***      +++   .       |            
#              |      .   ***      +++   .       |            
#              |      .   ***      +++   .       |            
#              |      .  *****    +++++  .       |            
#              |      . *******  +++++++ .       |           
#              |      .*********+++++++++.       |            
#              |      .   <->            .       |
#              |      . interthickness           |
#              |      .<-------><------->.       |
#              |      .          thickness       |
#              |<---->.                  .<----->|
#              |   p0 .                  . q0    |
#
#




tf = Shadow.CompoundOE(name = 'transfocator_snigirev1996')
p0 = [0.0,0.0] # alternatively set to [3000.0,0.0] and do not use tf.add_drift_space_upstream(3000.0)
q0 = [0.0,0.0] # alternatively set to [0.0,189.87] and do not use tf.add_drift_space_downstream(189.87)
surface_shape =  [1,1] #  1=Sphere, 2=Ellipsoid, 4=Paraboloid, 5=Plane, 7=Hyperboloid

tf.append_transfocator(p0, q0, nlenses=[20,10], slots_empty=[0,0], radius=[300e-4,300e-4], thickness=[600e-4,600e-4], 
    interthickness=[25e-4,25e-4],
    surface_shape=surface_shape, convex_to_the_beam=0, diameter=[600e-4,600e-4], cylinder_angle=[0.0,0.0], 
    prerefl_file=None, refraction_index=[0.99999720,0.99999720], attenuation_coefficient=[28.0,28.0], 
    use_ccc=0)

tf.add_drift_space_upstream(3000.0)
tf.add_drift_space_downstream(189.87)

beam.traceCompoundOE(tf,write_start_files=iwrite,write_end_files=iwrite,write_star_files=iwrite)

if iwrite: beam.write("final.01")

Shadow.ShadowTools.plotxy(beam,1,3,nolost=1,nbins=151,yrange=[-0.01,0.01])
#END transfocator_snigirev1996.py


------------------------------------------------------------------------
