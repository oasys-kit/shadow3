# -*- coding: utf-8 -*-
"""Build and install Shadow3"""

#
# Memorandum: 
#
# Install from sources: 
#     git clone https://github.com/srio/shadow3
#     cd shadow3
#     python setup.py sdist build
#     python -m pip install -e . --no-deps --no-binary :all:
#     # pip install .
#
# Upload to pypi (when uploading, increment the version number):
#     python setup.py register (only once, not longer needed)
#     python setup.py sdist [upload]
#     python setup.py bdist_wheel
#     twine upload dist/*
#          
# Install from pypi:
#     pip install shadow3
#

#
# if problems compiling in new MacOS (>=10.10):
# export MACOSX_DEPLOYMENT_TARGET=10.9
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
    library_dirs=[]
    extra_link_args = []
else:
    compile_options = "_COMPILE4WIN"
    library_dirs=[]
    extra_link_args = []

setup(
    name='shadow3',
    version='18.1.24',
    packages=['Shadow'],
    url='http://github.com/srio/shadow3',
    license='http://www.gnu.org/licenses/gpl-3.0.html',
    author='Franco Cerrina and Manuel Sanchez del Rio',
    author_email='srio@esrf.eu',
    description='SHADOW is an open source ray tracing code for modeling optical systems.',
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
            libraries=['shadow3c', 'gfortran'],
        ),
    ],
)
