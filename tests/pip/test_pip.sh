#!/bin/bash

#===============================================================================
#
# script to test pip install shadow3
#
#===============================================================================
#
#


#
# step 1: create and start python3 virtual environment
#
source shadow1env/bin/deactivate
rm -rf shadow1env

virtualenv -p python3 --system-site-packages shadow1env
source shadow1env/bin/activate

#
# step 2: install shadow3
#
pip install shadow3

#
# some test
#
python ../script1.py


echo "All done. "
