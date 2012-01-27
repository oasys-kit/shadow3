
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     CRLs in SHADOW...   Beta version, still in development....
     
              version 1: 2012-01-27  srio@esrf.eu
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


This is an example to run a CRL in SHADOW. 

Note: This example shows an optical system that contains only one CRL.
      For the moment, it is not easy to insert a CRL in between other oe's.


Eg: a CRL of TWO lenses or FOUR interfaces:

                                                                          
                                                                          
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
<-------pp------------>                             <----- qq ------->

 p0: physical focal source-crl distance     
 q0: physical focal crl-image distance     
 pp: focal source-crl distance     
 qq: focal crl-image distance    
 ddIn: lens thickness [along optical axis, in material] ?    
 ddV:  lens length [along optical axis, in vacuum] ?    


Example 1: The CRL of Snigirev et al. Nature 384, p49 (1996)
============================================================

data: 
    source: 
       E = 14 keV
       FWHM = 150 um or Sigma = 63.8 um
       Divergence: conical, angle=10e-6 rad
 
    Lens: 
       N=30 holes, therefore 60 interfaces
       R=300 um
       ddIn = 25 um
       ddV = 2 R = 600 um
       Aluminium: n=1-delta; delta = 2.8e-6; mu=28cm^-1
    Distances:
       F = R/2/N/delta = 178.57 cm
          To obtain this F, we use (1/F=1/pp+1/qq):
             pp = 3000 cm
             qq = 189.87
    

STEP 1: Run the preprocessor (precrl) to create the SHADOW input file crl.01

shadow3 < crl_snigirev1996.inp 

where: 
---------------------  start crl_snigirev1996.inp --------------------------
precrl
1
1
0
60
1
0.9999972
0.0000000
28.000000
3000
189.87342
3000
189.87342
25e-4
600e-4
3

exit

---------------------  end crl_snigirev1996.inp --------------------------


2) Run SHADOW (runcrl)

shadow3 runcrl


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

