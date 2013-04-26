#
# script to set library and python path to be able to run python-shadow
# from any directory 
#
# customize it to your needs...
#

MYSHADOWDIR=/scisoft/xop2.3/extensions/shadowvui/shadow3
MYXRAYLIBDIR=/users/srio/xraylib-2.16.0
MYSRWDIR=/users/srio/GIT/trunk.r226

#needed for python/shadow
export  LD_LIBRARY_PATH=$MYSHADOWDIR #:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=$MYSHADOWDIR/build/lib.linux-x86_64-2.7 #:"${PYTHONPATH}"

#needed for python/xraylib
export  LD_LIBRARY_PATH=$MYXRAYLIBDIR/src/.libs:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=$MYXRAYLIBDIR/python:"${PYTHONPATH}"
export  DYLD_LIBRARY_PATH=$MYXRAYLIBDIR/src/.libs"${DYLD_LIBRARY_PATH}"

#needed for python/srw
export  LD_LIBRARY_PATH=$MYSRWDIR/work/srw_python:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=$MYSRWDIR/work/srw_python:"${PYTHONPATH}"

echo " "
echo "************************************************************"
echo "                  setpythonpath.sh:                         "
echo "LD_LIBRARY_PATH is: "
echo  $LD_LIBRARY_PATH
echo "PYTHONPATH is: "
echo $PYTHONPATH
echo "DYLD_LIBRARYPATH is:"
echo $DYLD_LIBRARY_PATH
echo "Done"
echo "************************************************************"
echo " "

