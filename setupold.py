import os, sys, string, re
from glob import glob
import numpy

import distutils
from distutils.core import setup, Extension

headers = glob (os.path.join ("src/def","*.h") )
#header = headers + glob (os.path.join ("Include/numpy","*.h") )

setup ( name = "Shadow",
        version = "0.1.0",
	packages=["Shadow"],
        package_dir={"Shadow":"./Shadow"},
        ext_modules = [Extension('Shadow.ShadowLib',
                                 ['src/c/shadow_bind_python.c'],
                                 include_dirs  = ['./src/', numpy.get_include()],
                                 library_dirs  = ['./src'],
                                 libraries     = ['shadow3','shadow3c'],
                                 extra_compile_args = ['-msse','-msse2'],
                                 extra_link_args = ['-msse','-msse2']
                                ),
                      ]
        )

