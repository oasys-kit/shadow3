import os, sys, string, re
from glob import glob
import numpy

import distutils
from distutils.core import setup, Extension

headers = glob (os.path.join ("Include","*.h") )
header = headers + glob (os.path.join ("Include/numpy","*.h") )

setup (	name = "Shadow", 
	version = "1.0",
	ext_modules = [Extension('Shadow', 
				['Shadow_python.c',], 
				library_dirs  = ['.'],
				libraries     = ['shadowc'],
				include_dirs  = ['.',numpy.get_include()],
				),
			]
	)

