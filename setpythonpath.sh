#
# script to set library and python path to be able to run python-shadow
# from any directory 
#
# customize it to your needs...
#

#needed for python/shadow
#export  LD_LIBRARY_PATH=/scisoft/xop2.3/extensions/shadowvui/shadow3:"${LD_LIBRARY_PATH}"
#export  PYTHONPATH=/scisoft/xop2.3/extensions/shadowvui/shadow3/:"${PYTHONPATH}"
export  LD_LIBRARY_PATH=/users/srio/GIT/shadow3/ #:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=/users/srio/GIT/shadow3/build/lib.linux-x86_64-2.7 #:"${PYTHONPATH}"

#needed for python/xraylib
export  LD_LIBRARY_PATH=/users/srio/xraylib-2.16.0/src/.libs:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=/users/srio/xraylib-2.16.0/python:"${PYTHONPATH}"
export  DYLD_LIBRARY_PATH=/users/srio/xraylib-2.16.0/src/.libs"${DYLD_LIBRARY_PATH}"

#needed for python/srw
export  LD_LIBRARY_PATH=/users/srio/GIT/trunk.r226/work/srw_python:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=/users/srio/GIT/trunk.r226/work/srw_python:"${PYTHONPATH}"

echo "LD_LIBRARY_PATH is: $LD_LIBRARY_PATH"
echo "PYTHONPATH is: $PYTHONPATH"
echo "DYLD_LIBRARYPATH: $DYLD_LIBRARY_PATH"
echo "setpythonpath.sh done"

