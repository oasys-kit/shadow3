
#
#
# setup.py for shadow3
# Usage:
#
#
# # for macOSX
#
# python3 setup.py sdist
# python3 setup.py bdist_wheel
# cp src/libshadow*.so .
# delocate-wheel -w fixed_wheels -v dist/Shadow-0.1.0-cp36-cp36m-macosx_10_9_x86_64.whl
# python3 -m pip install fixed_wheels/Shadow-0.1.0-cp36-cp36m-macosx_10_9_x86_64.whl
#
#

# In windows:
# I patched the file anaconda3/Lib/distutils/cygwinccompiler.py
# elif int(msc_ver) >= 1900:
# # VS2015 / MSVC 14.0
# return []  # ['msvcr140']
# python3 setup.py bdist_wheel
# do manuallu:
# rename .whl to .zip
# add libshadow3.dll and libshadow3c.dll to Shadow in the zipp file
# rename back .zip to .whl


import os, sys, string, re
from glob import glob
import numpy

import distutils
from distutils.core import setup, Extension
from distutils.command.build import build
from shutil import which
from subprocess import run as sub_run
from subprocess import Popen

import setuptools
from setuptools import setup, Extension
# from distutils.core import setup

headers = glob (os.path.join ("src/def","*.h") )
#header = headers + glob (os.path.join ("Include/numpy","*.h") )
here = os.path.abspath(os.path.dirname(__file__))

def check_dependencies():
    '''
    Check dependencies for Windows (msbuild) and Unix (make).
    :raises Exception: if one is missing, an exception is raised.
    '''
    if sys.platform == 'win32':
        if not which("gfortran"):
            raise Exception("You need to install the gfortran compiles (in conda mingw32)")
    else:
        if not which("make"):
            raise Exception("You need to install make in order to execute the makefile to build Shadow")


class ShadowBuild(build):
    '''
    This class is a wrapper to build Shadow before making link before Shadow
    '''
    def run(self):
        check_dependencies()
        if sys.platform == 'win32':
            path_to_bat = os.path.join(here, 'src')
            bat_name = "make.bat"
            try:
                batch_process = Popen(bat_name, cwd=path_to_bat, shell=True)
                stdout, stderr = batch_process.communicate()
                if stderr:
                    raise Exception('An error occur during Shadow compilation. Message: {}'.format(stderr))
            except OSError as err:
                raise OSError('{} should be located on src. Current path: {}'.format(bat_name, path_to_bat))
            super().run()
        else:
            sub_run(['make', '-C', os.path.join(here, 'src'), 'clean'])
            sub_run(['make', '-C', os.path.join(here, 'src'), 'lib'])
            super().run()
            # sub_run(['make', '-C', os.path.join(here, 'src'), 'clean'])


setup ( name = "shadow3",
        version = "0.1.0",
	    packages=["Shadow"],
        package_dir={"Shadow":"./Shadow"},
        # package_data={'Shadow':['libshadow3.so','libshadow3c.so']},
        cmdclass={'build': ShadowBuild},
        # include_package_data=True,
        ext_modules = [Extension('Shadow.ShadowLib',
                                 ['src/c/shadow_bind_python.c'],
                                 include_dirs  = ['src/def', numpy.get_include()],
                                 library_dirs  = ['./src'],
                                 libraries     = ['shadow3','shadow3c'],
                                 # extra_compile_args = ['-msse','-msse2'],
                                 # extra_link_args = ['-msse','-msse2'],
                                ),
                      ],
        # data_files=[('src', ['src/libshadow3.so','src/libshadow3c.so'])],
        )

