# coding: utf-8
# /*##########################################################################
#
# Copyright (c) 2022 European Synchrotron Radiation Facility
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# ###########################################################################*/

#
# Memorandum:
#
# Install from sources:
#     git clone https://github.com/oasys-kit/shadow3
#     cd shadow3
#     ./make_mac.sh
##################################################################
#  >>>>>>>>>>>>>>>>>> python setup_old.py build --compiler=cygwin
##################################################################
#     python -m pip install -e . --no-deps --no-binary :all:
#
# Upload to pypi (when uploading, increment the version number):
#     python setup.py register (only once, not longer needed)
#     python setup.py sdist
#     python setup.py upload
#
# Install from pypi:
#     pip install <name>
#

__authors__ = ["M Sanchez del Rio"]
__license__ = "MIT"
__date__ = "2022"

import os
import numpy

# from distutils.core import setup, Extension
from setuptools import setup, Extension

# try:
#     from setuptools import find_packages, setup, Extension
# except AttributeError:
#     from setuptools import find_packages, setup, Extension

NAME = 'shadow3'

VERSION = '22.2.2'
ISRELEASED = True

DESCRIPTION = 'shadow3'
README_FILE = os.path.join(os.path.dirname(__file__), 'README.md')
LONG_DESCRIPTION = open(README_FILE).read()
AUTHOR = 'Manuel Sanchez del Rio'
AUTHOR_EMAIL = 'srio@esrf.eu'
URL = 'https://github.com/oasys-kit/shadow3'
DOWNLOAD_URL = 'https://github.com/oasys-kit/shadow3'
MAINTAINER = 'Manuel Sanchez del Rio'
MAINTAINER_EMAIL = 'srio@esrf.eu'
LICENSE = 'MIT'

KEYWORDS = [
    'ray tracing',
    'x-rays',
    'synchrotron radiation',
    'simulation',
]

CLASSIFIERS = [
    'Development Status :: 5 - Production/Stable',
    'Environment :: Console',
    'Environment :: Plugins',
    'Programming Language :: Python :: 3',
    'License :: OSI Approved :: '
    'GNU General Public License v3 or later (GPLv3+)',
    'Operating System :: POSIX',
    'Operating System :: Microsoft :: Windows',
    'Topic :: Scientific/Engineering :: Visualization',
    'Topic :: Software Development :: Libraries :: Python Modules',
    'Intended Audience :: Education',
    'Intended Audience :: Science/Research',
    'Intended Audience :: Developers',
]

INSTALL_REQUIRES = (
    'setuptools',
    'numpy',
)

SETUP_REQUIRES = (
    'setuptools',
)


PACKAGE_DATA = {
    "Shadow": ["*.txt"],
}

EXT_MODULES = [Extension('Shadow.ShadowLib',
                 ['src/c/shadow_bind_python.c'],
                 include_dirs  = ['.', numpy.get_include(),'src/c', 'src/def'],
                 library_dirs  = ['.'],
                 libraries     = ['shadow3','shadow3c'],
                 extra_compile_args = ['-msse','-msse2'],
                 extra_link_args = ['-msse','-msse2']
                ),
                ]

PACKAGES = [
    "Shadow"
    ]

DEFINE_MACROS = [("NPY_NO_DEPRECATED_API", True)]

def setup_package():
    setup(
        name=NAME,
        version=VERSION,
        description=DESCRIPTION,
        long_description=LONG_DESCRIPTION,
        author=AUTHOR,
        author_email=AUTHOR_EMAIL,
        maintainer=MAINTAINER,
        maintainer_email=MAINTAINER_EMAIL,
        url=URL,
        download_url=DOWNLOAD_URL,
        license=LICENSE,
        keywords=KEYWORDS,
        classifiers=CLASSIFIERS,
        packages=PACKAGES,
        package_data=PACKAGE_DATA,
        zip_safe=False,  # the package can run out of an .egg file
        include_package_data=True,
        install_requires=INSTALL_REQUIRES,
        setup_requires=SETUP_REQUIRES,
        ext_modules=EXT_MODULES,
        # define_macros=DEFINE_MACROS,
    )

if __name__ == '__main__':
    setup_package()




# import os, sys, string, re
# from glob import glob
# import numpy
#
# import distutils
# #from distutils.core import setup, Extension
# from setuptools import setup, Extension
#
# headers = glob (os.path.join ("Include","*.h") )
# #header = headers + glob (os.path.join ("Include/numpy","*.h") )
#
# setup ( name = "shadow3",
#         version = "22.1.31",
# 	packages=["Shadow"],
#         package_dir={"Shadow":"."},
#         ext_modules = [Extension('Shadow.ShadowLib',
#                                  ['shadow_bind_python.c'],
#                                  include_dirs  = ['.', numpy.get_include()],
#                                  library_dirs  = ['.'],
#                                  libraries     = ['shadow3','shadow3c'],
#                                  extra_compile_args = ['-msse','-msse2'],
#                                  extra_link_args = ['-msse','-msse2']
#                                 ),
#                       ]
#         )

