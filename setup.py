# -*- coding: utf-8 -*-
"""Build and install Shadow3"""
from __future__ import division, absolute_import, print_function

import datetime
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
#srio
from distutils.core import setup
import distutils.cmd

#from pykern import pksetup

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

#     def __version_h(self):
#         self.mkpath(self.build_clib)
#         t = '''{hline}
#     {header:^80}
#     {hline}
# 
#     {prefix}Date:
#     {date}
# 
#     {prefix}Compiler:
#     {compiler}
# 
#     {prefix}Platform:
#     {platform}
# 
#     {prefix}Build:
#     {build}
# 
#     {hline}'''
#         out = t.format(
#             # We can't show c compiler, because there's no such string
#             # in distutils. msvccompiler, for example, creates the string
#             # in the spawn() call. This tells us enough
#             compiler=self._f_compiler.compiler_f90,
#             date=datetime.datetime.utcnow().ctime() + ' UTC',
#             header='compilation settings',
#             hline='+' * 80,
#             platform=platform.platform(),
#             prefix=' ' + ('+' * 5),
#             build=self.distribution.version,
#         ).rstrip()
#         lines = []
#         for line in out.split('\n'):
#             if not line:
#                 line = ' '
#             line = 'print *,"{}"\n'.format(line)
#             lines.append(line)
#         out = os.path.join(self.build_clib, 'shadow_version.h')
#         with open(out, 'w') as f:
#             f.write(''.join(lines))
#         return out


#pksetup.setup(
setup(
    name='shadow3',
    packages=['Shadow'],
    url='http://github.com/srio/shadow3',
    license='http://www.gnu.org/licenses/gpl-3.0.html',
    author='Franco Cerrina and M. Sanchez del Rio',
    author_email='srio@esrf.eu',
    description='SHADOW is an open source ray tracing code for modeling optical systems.',
    #pksetup={  TODO: this gives a warning
    setup={
        'extra_directories': ['c', 'def', 'fortran'],
    },
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
            'macros': [('_COMPILE4NIX', 1)],
            'include_dirs': ['src/def', 'src/fortran', 'src/c'],
            # Can't use extra_compiler_args, because applied to all
            # compilers, and some flags are only used. See BuildClib
        }),
    ],
    cmdclass={
        'build_clib': BuildClib,
        #'build_src': pksetup.NullCommand,
        'build_src': NullCommand,
    },
    ext_modules=[
        setuptools.Extension(
            name='Shadow.ShadowLib',
            sources=['src/c/shadow_bind_python.c'],
            include_dirs=['src/c', 'src/def', numpy.get_include()],
            libraries=['shadow3c', 'gfortran'],
        ),
    ],
)
