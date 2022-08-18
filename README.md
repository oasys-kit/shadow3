# SHADOW 3.0 SOURCE DISTRIBUTION


## Contents:


1. What is SHADOW
2. Download
3. Source files
4. Building SHADOW
5. Other info
6. Contact

## 1 What is SHADOW

SHADOW is an open source ray tracing code for modeling optical systems. 

Targeted to synchrotron radiation beamlines, it has unique features for 
designing X-ray optical systems. 

For more info, please read this paper (open access):

SHADOW3: a new version of the synchrotron X-ray optics modelling package: 
- M. Sanchez del Rio, N. Canestrari, F. Jiang and F. Cerrina
Journal of Synchrotron Radiation Volume 18, Part 5 (September 2011)
http://dx.doi.org/10.1107/S0909049511026306

If you are (or want to be) a SHADOW user, it is recommended that you use
a user interface. We strongly recommend OASYS (https://www.aps.anl.gov/Science/Scientific-Software/OASYS) with its ShadowOui add-on. 
Information can ba found at these papers: 
- Luca Rebuffi, Manuel Sanchez del Rio (2016)  ShadowOui : a new visual environment for X-ray optics and synchrotron beamline simulations Journal of Synchrotron Radiation 23:  6.  1357-1367 http://dx.doi.org/10.1107/S1600577516013837
- X J Yu, X Chi, T Smulders, A T S Wee, A Rusydi, M Sanchez del Rio, M B H Breese (2022)  Beamline simulations using monochromators with high d-spacing crystals Journal of Synchrotron Radiation 29:  5. https://doi.org/10.1107/S160057752200707X

Note also that the ShadowOUI interface implements the hybrid ray-tracing method explained at: 
- Xianbo Shi, Ruben Reininger, Manuel Sanchez del Rio, Lahsen Assoufid (2014)  A hybrid method for X-ray optics simulation : combining geometric ray-tracing and wavefront propagation Journal of Synchrotron Radiation 21:  4.  669-678.   http://dx.doi.org/10.1107/S160057751400650X
- Xianbo Shi, Manuel Sanchez del Rio, Ruben Reininger (2014)  A new SHADOW update : integrating diffraction effects into ray-tracing SPIE Proceedings 9209:  920911-920911-9.  http://dx.doi.org/10.1117/12.2061984
- Xianbo Shi, Ruben Reininger, Manuel Sánchez del Río, Jun Qian, Lahsen Assoufid (2014)  X-ray optics simulation and beamline design using a hybrid method : diffraction-limited focusing mirrors SPIE Proceedings 9209:  920909-920909-9.  http://dx.doi.org/10.1117/12.2061950

If you want to cite the first paper on Shadow by Franco Cerrina, please cite: 
-  F. Cerrina "Ray Tracing Of Recent VUV Monochromator Designs", Proc. SPIE 0503, Application, Theory, and Fabrication of Periodic Structures, DiffractionGratings, and Moire Phenomena II, (12 December 1984); https://doi.org/10.1117/12.944815  

## 2 Download

SHADOW3 sources are downloaded using git: 

  ```
  git clone https://github.com/oasys-kit/shadow3
  ```

Other SHADOW web resources: 

  - shadow3 binary distributions (wheels): https://pypi.org/project/shadow3/

  - Documentation:
     - Shadow Primer (for using shadow3 in command mode)
      http://ftp.esrf.eu/pub/scisoft/shadow3/Shadow3Primer.pdf

  - Official reference:
    http://dx.doi.org/10.1107/S0909049511026306 


## 3 Important files

- in shadow3/src: fortran, C and other sources
- in Shadow: python sources

- in docs: README files:
  - README_API.txt 
  - README_CRL.txt 
  - README_MPI.txt 
  - README_PYTHON.txt
  - source.nml   (description of the source variables, as in start.00)
  - oe.nml       (description of the optical element variables, as in start.01)

- in data: data files: 
  - PRELIB1.DAT and PRELIB2.DAT (data file containg the oprical library)

## 4 Building SHADOW
  binary:
  ```
  cd src
  # Check (edit) the Makefile if you need to redefine compilers etc.
  # The use of Makefile is documented in its header. Basically:
  make           : builds shadow3 the main program
  make all       : builds shadow3, libraries, etc
  make install   : installation (see Makefile)
  ```

  python API:
  
  in shadow3 directory, use the standard python setup (see comments in setup.py for specific platforms):
  
  ```
  python setup.py sdist build
  python setup.py bdist_wheel
  python -m pip install dist/...whl
   ```

## 5 Other info 
  - Other documents, mostly obsolete, can be found at https://github.com/srio/shadow3-docs
  
## 6 Contact

SHADOW is currently maintained by Manuel Sanchez del Rio (srio@esrf.eu)



