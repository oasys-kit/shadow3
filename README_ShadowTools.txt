                                  ShadowTools
                                  ===========

Shadow3 graphic applications (plotxy and histo1) in python/matplotlib

                                  by 
           Niccolo Canestrari (niccolo.canestrari@gmail.com) 
                                  and 
                    Manuel Sanchez del Rio (srio@esrf.eu)


REQUIREMENTS
============

1) needs shadow3 compiled (including python modules)
2) needs python (we use ipython) with matplotlib installed.


INITIALIZATION
==============

For creating test files: 
 1) Create a TEST directory under shadow3
 2) in shadow3/TEST start ShadowVUI and run ex17a.ws 
    You should create the si5_15.ws file before (using the default setup 
    in bragg)

Launch python in the shadow3 directory: 

ipython -pylab qt  

import ShadowTools as st


HISTO1 examples
===============

st.histo1("TEST/begin.dat",1)

st.histo1("TEST/begin.dat",1,nbins=80,xrange=[-.15,.15]) 

st.histo1("TEST/begin.dat",1,nbins=80,yrange=[0.0,300.0])

st.histo1("TEST/begin.dat",1,nbins=80,nolost=2)

st.histo1("TEST/begin.dat",1,nbins=80,ref=23)

st.histo1("TEST/begin.dat",1,write=1)

st.histo1("TEST/begin.dat",1,nbins=100,title="Hello",xtitle="Hello1",ytitle="Hello2")

st.histo1("TEST/begin.dat",11,noplot=1)
st.plt.show() 


p = st.histo1("TEST/begin.dat",11,noplot=1)
p.<TAB> to see output
p.figure.show()

st.histo1("TEST/star.01",11,ref=1)


PLOTXY examples
===============

help(st.plotxy)

st.plotxy("TEST/begin.dat",1,4,xrange=[-0.25,0.2])

st.plotxy("TEST/begin.dat",1,4,yrange=[-.002,0.002])
  -> crash!!


! I modifies oe1 to set limits in the -2,2 range 
st.plotxy("TEST/begin.dat",1,3,nolost=1)
st.plotxy("TEST/begin.dat",1,3,nolost=2)


st.plotxy("TEST/star.01",1,2,xtitle="Hello",ytitle="hello2",title="Hello")


st.plotxy("TEST/star.01",1,2,noplot=1)
st.plt.show()

st.plotxy("TEST/star.01",1,2,calfwhm=1)

o= st.plotxy("TEST/star.01",1,2,calfwhm=1)

st.plotxy("TEST/star.02",1,3,contour=5)  

st.plotxy("TEST/star.02",1,3,contour=6)


OTHER examples
==============

a=st.getshcol("TEST/star.01",(1,3))
a=st.getshcol("TEST/star.01",1)
( also a=st.getshonecol("TEST/star.01",1) )


FAQ
===

1) cannot use TAB in ipython: set correct ~/.ipython files

2) how to print the plot?  Save to a file, and print it

3) how to clean MATPLOTLIB memory (clicking "x" in the window does not clean
   memory): st.plt.close("all")

