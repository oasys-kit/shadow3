
#
# import to the main level the main objects first defined in C (ShadowLib) 
# then extended in ShadowLibExtensions
#
#from Shadow import ShadowLib
from __future__ import print_function
from Shadow.ShadowLibExtensions import OE, Source, Beam, CompoundOE, IdealLensOE

# Defined in C, not used at main level
#from Shadow.ShadowLib import saveBeam, FastCDFfromZeroIndex, FastCDFfromOneIndex, FastCDFfromTwoIndex

#
# import optional packages: 
#     ShadowTools: graphic application (plotxy, etc)
#     ShadowPreprocessorsXraylib: preprocessors (bragg, etc) using Xraylib
#     ShadowSrw: Srw+Shadow binding
#
import sys
try:
    import Shadow.ShadowTools as ShadowTools
except ImportError:
    print(sys.exc_info()[1]) 
    pass
try:
    import Shadow.ShadowPreprocessorsXraylib as ShadowPreprocessorsXraylib
except ImportError:
    print(sys.exc_info()[1]) 
    pass
#try:
#    import Shadow.ShadowSrw as ShadowSrw
#except:
#    print(sys.exc_info()[1]) 
#    pass

