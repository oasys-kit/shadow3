#
# script to set library and python path to be able to run 
# SHADOW, XRAYLIB and SRW from any directory 
#
#
# The three packages are obtained by git and built locally (not installed): 
#    git clone git://git.epn-campus.eu/repositories/shadow3 
#    git clone https://github.com/ochubar/SRW.git
#    git clone https://github.com/tschoonj/xraylib.git
#
# customize it to your needs...
#    1) set the paths at the beginning of the script
#    2) look at the "TODO" in the script to check for additional changes
#
# Usage: 
#        . setpythonpath.sh [init]
#
#


# define paths for the software packages
MYSHADOWDIR=/scisoft/users/srio/Working/XOPPY/shadow3
MYXRAYLIBDIR=/buffer/srio/xraylib
MYSRWDIR=/buffer/srio/SRW
#define python call (for tests)
PY=python
#PY=python3

# be sure that the variables do exist
if [ $1 == "init" ]; then
    LD_LIBRARY_PATH=""
    PYTHONPATH=""
    DYLD_LIBRARY_PATH=""
    echo "INITIALIZING env vars"
else
    # initialize variables if undefined
    if [ -z "$LD_LIBRARY_PATH" ]; then
        LD_LIBRARY_PATH=""
    fi 
    if [ -z "$PYTHONPATH" ]; then
        PYTHONPATH=""
    fi 
    if [ -z "$DYLD_LIBRARY_PATH" ]; then
        DYLD_LIBRARY_PATH=""
    fi 
fi

#needed for python/shadow
export  LD_LIBRARY_PATH=$MYSHADOWDIR:"${LD_LIBRARY_PATH}"
#TODO: change these directories if needed!!
export  PYTHONPATH=$MYSHADOWDIR/build/lib.linux-x86_64-2.7:"${PYTHONPATH}"
#export  PYTHONPATH=$MYSHADOWDIR/build/lib.linux-x86_64-3.2/:"${PYTHONPATH}"
export  DYLD_LIBRARY_PATH=$MYSHADOWDIR/build/lib.macos:"${DYLD_LIBRARY_PATH}"

#needed for python/xraylib
export  LD_LIBRARY_PATH=$MYXRAYLIBDIR/src/.libs:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=$MYXRAYLIBDIR/python/.libs:"${PYTHONPATH}"
export  DYLD_LIBRARY_PATH=$MYXRAYLIBDIR/src/.libs:"${DYLD_LIBRARY_PATH}"

#needed for python/srw
export  LD_LIBRARY_PATH=$MYSRWDIR/cpp/gcc:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=$MYSRWDIR/env/work/srw_python:"${PYTHONPATH}"
export  DYLD_LIBRARY_PATH=$MYSRWDIR:"${DYLD_LIBRARY_PATH}"

#this is for testing 
echo " "
echo "************************************************************"
echo "                  setpythonpath.sh:                         "
echo "  "
echo "Testing Shadow:"
$PY -c "import Shadow"
echo "Done"
#echo "  "
#echo "Testing xraylib:"
#$PY -c "import xraylib"
#echo "Done"
#echo "  "
#echo "Testing SRW:"
#$PY -c "import srwlib"
#echo "Done"
echo "  "
echo "LD_LIBRARY_PATH is: "
echo  $LD_LIBRARY_PATH
echo "  "
echo "PYTHONPATH is: "
echo $PYTHONPATH
echo "  "
echo "DYLD_LIBRARYPATH is:"
echo $DYLD_LIBRARY_PATH
echo "  "
echo "Done"
echo "  "
echo "************************************************************"
echo " "

