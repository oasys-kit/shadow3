import os, sys, string, re
from glob import glob
import numpy

import distutils
from distutils.core import setup, Extension
from distutils.command.build_ext import build_ext


class BuildExt(build_ext):
    """Handle extension compilation.

    Command-line argument and environment can custom:

    - The use of cython to cythonize files, else a default version is used
    - Build extension with support of OpenMP (by default it is enabled)
    - If building with MSVC, compiler flags are converted from gcc flags.
    """

    COMPILE_ARGS_CONVERTER = {'-fopenmp': '/openmp'}

    LINK_ARGS_CONVERTER = {'-lmsvcr140': ''}

    description = 'Build extensions'

    print(">>>>>>>>>>>>> HERE 333 >>>>>>>>>>>>>>>>>>>>>>>>")

    def patch_extension(self, ext):
        """
        Patch an extension according to requested Cython and OpenMP usage.

        :param Extension ext: An extension
        """
        # Cytonize
        print(">>>>>>>>>>>>> HERE 222 >>>>>>>>>>>>>>>>>>>>>>>>",dir(ext))
        print(">>>>>>>>>>>>><<<<<<<<<<<<< linker-----:",list(self.compiler.linker_so))
        # Convert flags from gcc to MSVC if required
        if True:
            extra_compile_args = [self.COMPILE_ARGS_CONVERTER.get(f, f)
                                  for f in ext.extra_compile_args]
            print(">>>>>> extra_compiler_args: ",extra_compile_args)
            # Avoid empty arg
            ext.extra_compile_args = [arg for arg in extra_compile_args if arg]

            extra_link_args = [self.LINK_ARGS_CONVERTER.get(f, f)
                               for f in ext.extra_link_args]
            print(">>>>>> extra_link_args: ",extra_link_args)
            # Avoid empty arg
            ext.extra_link_args = [arg for arg in extra_link_args if arg]

    def patch_compiler(self):
        """
        Patch the compiler to:
        - always compile extensions with debug symboles (-g)
        - only compile asserts in debug mode (-DNDEBUG)

        Plus numpy.distutils/setuptools/distutils inject a lot of duplicated
        flags. This function tries to clean up default debug options.
        """
        print(">>>>>>>>>>>>> HERE >>>>>>>>>>>>>>>>>>>>>>>>")

        if True:
            print(">>>>>>>>>>>>><<<<<<<<<<<<< linker-----:",list(self.compiler.linker_so))
            args = list(self.compiler.compiler_so)
            print(">>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<",args)
            # clean up debug flags -g is included later in another way
            must_be_cleaned = ["-DNDEBUG", "-g"]
            args = filter(lambda x: x not in must_be_cleaned, args)
            args = list(args)

            # patch options
            self.compiler.compiler_so = list(args)

    def build_extensions(self):
        self.patch_compiler()
        for ext in self.extensions:
            self.patch_extension(ext)
        build_ext.build_extensions(self)

headers = glob (os.path.join ("src/def","*.h") )
#header = headers + glob (os.path.join ("Include/numpy","*.h") )

cmdclass = dict(
    # build=Build,
    # build_py=build_py,
    # test=PyTest,
    # build_screenshots=BuildDocAndGenerateScreenshotCommand,
    # build_doc=BuildDocCommand,
    # test_doc=TestDocCommand,
    build_ext=BuildExt,
    # build_man=BuildMan,
    # clean=CleanCommand,
    # sdist=SourceDistWithCython,
    # debian_src=sdist_debian,
    )

setup ( name = "Shadow",
        version = "0.1.0",
	    packages=["Shadow"],
        package_dir={"Shadow":"./Shadow"},
        cmdclass=cmdclass,
        ext_modules = [Extension('Shadow.ShadowLib',
                                 ['src/c/shadow_bind_python.c'],
                                 include_dirs  = ['./src/def', numpy.get_include()],
                                 library_dirs  = ['./src'],
                                 libraries     = ['shadow3','shadow3c'],
                                 #extra_compile_args = ['-msse','-msse2'],
                                 #extra_link_args = ['-lmsvcr1400','-msse2'],
                                 extra_link_args = [],
                                ),
                      ],
        data_files=[('ll', ['src/libshadow3.so','src/libshadow3c.so'])],
        )

