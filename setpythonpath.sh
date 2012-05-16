#
# script to set library and python path to be able to run python-shadow
# from any directory 
#
# customize it to your needs...
#

export  LD_LIBRARY_PATH=/scisoft/xop2.3/extensions/shadowvui/shadow3:"${LD_LIBRARY_PATH}"
export  PYTHONPATH=/scisoft/xop2.3/extensions/shadowvui/shadow3/:"${PYTHONPATH}"

echo "LD_LIBRARY_PATH is: $LD_LIBRARY_PATH"
echo "PYTHONPATH is: $PYTHONPATH"
echo "setpythonpath.sh done"
