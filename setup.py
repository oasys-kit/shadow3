# -*- coding: utf-8 -*-
"""Build and install Shadow3"""

#
# Memorandum: 
#
# Install from sources: 
#     git clone https://github.com/oasys-kit/shadow3
#     cd shadow3
#     python setup.py sdist build
#     # install 
#     python -m pip install .
#     # or install using links (developer) 
#     python -m pip install -e . --no-deps --no-binary :all:
#     export LD_LIBRARY_PATH=`pwd` # (to find libshadow3.so and linshadow3.so)
#
# Create the wheel and upload to pip
#     python setup.py register (only once, not longer needed)
#     python setup.py sdist build
#     python setup.py bdist_wheel
#     export LD_LIBRARY_PATH=`pwd` # (to find libshadow3.so and linshadow3.so)
#     export PATH=/scisoft/manylinux1/bin:$PATH
#     auditwheel repair --plat manylinux_2_31_x86_64 dist/shadow3-22.6.3-cp37-cp37m-linux_x86_64.whl
#     python -m twine upload wheelhouse/*
#          
# Install from pypi:
#     python -m pip shadow3
#

####################################
# Notes on creating the linux wheels
####################################
# It is important not to mix libraries (C, Fortran) otherwise compilation or run-time problems appear.
# It is recommended to use the conda installation used by Oasys. Presently:
#
# https://repo.continuum.io/miniconda/Miniconda3-py37_4.10.3-Linux-x86_64.sh
#  and use compilers from conda:
#  conda install -c conda-forge gcc
#  conda install -c conda-forge gfortran
#
#
# some link problems in finding libraries were (quick-and-dirty) solved by: 
# sudo updatedb
# sudo cp --preserve=links /usr/lib/x86_64-linux-gnu/libpthread.so.0 /lib64/
# sudo cp --preserve=links /home/srio/miniconda3-py37/x86_64-conda-linux-gnu/sysroot/usr/lib64/libpthread_nonshared.a /usr/lib64/
# sudo cp --preserve=links /usr/lib/x86_64-linux-gnu/libc.so.6 /lib64
# sudo cp --preserve=links /usr/lib/x86_64-linux-gnu/libc_nonshared.a /usr/lib64/
#
#
#  conda 4.11.0
#  gcc (GCC) 11.2.0
#  GNU Fortran (GCC) 11.2.0
#  auditwheel 5.1.2
#  => shadow3-22.2.7-cp37-cp37m-manylinux_2_31_x86_64.whl
#
#
# if problems compiling in MacOS (>=10.10):
# export MACOSX_DEPLOYMENT_TARGET=10.9
#

###############################
# Notes creating windows wheels
###############################
#
# gfortran and gcc are installed within mingw64 (and not with conda)
# compilation works with miniconda but also with native python
#
# see https://stackoverflow.com/questions/6034390/compiling-with-cython-and-mingw-produces-gcc-error-unrecognized-command-line-o
# and edit <python_dir>\Lib\site-packages\setuptools\_distutils\cygwinccompiler.py
#          ( and may be also <python_dir>\Lib\disutils\cygwinccompiler.py ):
#                 i) suppress all "-mcygwin"
#                 ii) replace return ['iucr','vcruntime140] by return []  (this for python >3.7)
#
#
# step 1: create wheels
#
# python setup.py build --compiler=cygwin
# python setup.py bdist_wheel

# step 2: compile shadow3 and create dlls
#
# cd src\windows_gfortran
# compile_fortran.bat
# cd ..\..

# step 3: add the dlls to the wheels
#
# rename dist\...whl to .zip, open it and to in the Shadow directory:
#   libshadow3.dll, libshadow3c.dll, libgcc_s_sjlj-1.dll and libquadmath-0.dll
#   Rename back to .whl

# Note that step 2 and 3 can be automatically done running
#
# python create_and_pack_dlls.py
#


import glob
import os
import os.path
import pip
import platform
import setuptools
import setuptools.command.test
import subprocess
import sys

import numpy
from numpy.distutils.command.build_clib import build_clib
from distutils.core import setup
import distutils.cmd

class NullCommand(distutils.cmd.Command, object):
    """Use to eliminate a ``cmdclass``.
    Does nothing but complies with :class:`distutils.cmd.Command` protocol.
    """

    user_options = []

    def initialize_options(*args, **kwargs):
        pass

    def finalize_options(*args, **kwargs):
        pass

    @property
    def build_src(*args, **kwargs):
        return ""
    
    def run(*args, **kwargs):
        pass


class BuildClib(build_clib, object):
    """Set up for shadow3c build"""

    def finalize_options(self):
            try:
                build_clib.finalize_options(self)
            except AttributeError:
                pass
            

    def build_libraries(self, *args, **kwargs):
        """Modify the f90 compiler flags and build shadow_version.h"""
        f90 = self._f_compiler.compiler_f90
        # Is this portable?
        for f in ('-Wall', '-fno-second-underscore'):
            if f in f90:
                f90.remove(f)
        f90.extend(('-cpp', '-ffree-line-length-none', '-fomit-frame-pointer', '-I' + self.build_clib))
                #srio: not needed for python  
	#self.__version_h()
        return super(BuildClib, self).build_libraries(*args, **kwargs)



if sys.platform == 'darwin':
    compile_options = "_COMPILE4MAX"
    import subprocess
    #library_dirs=subprocess.check_output(["locate", "libgfortran.dylib"]).decode().replace("/libgfortran.dylib","").split("\n")[:-1]
    library_dirs=subprocess.check_output(["gfortran" , "--print-file-name" , "libgfortran.dylib"]).decode().replace("/libgfortran.dylib","").split("\n")[:-1]
    extra_link_args = ['-Wl,-no_compact_unwind']
    
elif sys.platform == 'linux':
    compile_options = "_COMPILE4NIX"
    library_dirs = []
    extra_link_args = []
else:
    compile_options = "_COMPILE4WIN"
    library_dirs = ["./"]
    # library_dirs=["./","/Users/srio/Miniconda3"]
    # library_dirs = ["./", "c:Miniconda3-py39"]
    extra_link_args = []

setup(
    name='shadow3',
    version='22.8.20',
    packages=['Shadow'],
    url='http://github.com/oasys-kit/shadow3',
    license='http://www.gnu.org/licenses/gpl-3.0.html',
    author='Franco Cerrina and Manuel Sanchez del Rio',
    author_email='srio@esrf.eu',
    description='SHADOW is an open source ray tracing code for modeling optical systems.',
    install_requires=['numpy'],
    libraries=[
        ('shadow3c', {
            'some-random-param': 1,
            'sources': [
                'src/c/shadow_bind_c.c',
                # The order of these files matters, because fortran
                # compilers need the "*.mod" files for "use" statements
                # 'fortran/shadow_version.f90',
                'src/fortran/shadow_globaldefinitions.f90',
                'src/fortran/stringio.f90',
                'src/fortran/gfile.f90',
                'src/fortran/shadow_beamio.f90',
                'src/fortran/shadow_math.f90',
                'src/fortran/shadow_variables.f90',
                'src/fortran/shadow_roughness.f90',
                'src/fortran/shadow_kernel.f90',
                'src/fortran/shadow_synchrotron.f90',
                'src/fortran/shadow_pre_sync.f90',
                'src/fortran/shadow_pre_sync_urgent.f90',
                'src/fortran/shadow_preprocessors.f90',
                'src/fortran/shadow_postprocessors.f90',
                'src/fortran/shadow_bind_f.f90',
                'src/fortran/shadow_crl.f90',
            ],
            'macros': [(compile_options, 1)],
            'include_dirs': ['src/def', 'src/fortran', 'src/c'],
            # Can't use extra_compiler_args, because applied to all
            # compilers, and some flags are only used. See BuildClib
        }),
    ],
    cmdclass={
        'build_clib': BuildClib,
        'build_src': NullCommand,
    },
    ext_modules=[
        setuptools.Extension(
            name='Shadow.ShadowLib',
            sources=['src/c/shadow_bind_python.c'],
            include_dirs=['src/c', 'src/def', numpy.get_include()],
            extra_link_args=extra_link_args,
            library_dirs=library_dirs,
            libraries=['shadow3c', 'gfortran', 'quadmath'],
        ),
    ],
)
