#
# M. Sanchez del Rio srio@esrf.eu
# ESRF 2011
#
# script to define environment for Shadow3 in connection with ShadowVUI
#

setenv XOP_HOME /scisoft/xop2.3
setenv  PATH $XOP_HOME/extensions/shadowvui/shadow3:$PATH
setenv  LD_LIBRARY_PATH $XOP_HOME/extensions/shadowvui/shadow3:$LD_LIBRARY_PATH
setenv  IDL_DLM_PATH $XOP_HOME/extensions/shadowvui/shadow3

