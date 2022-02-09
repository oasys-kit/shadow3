Wheels for tests
================

This directory contains shadow3 wheels for testing before release.

They have been built using the branch: 
https://github.com/oasys-kit/shadow3/tree/devel-gfortran-yb66


It included changes to compile with new gfortran compilers (thanks Tom Schoonjans)
and calculation of any crystal structure via a new bragg preprocessor file (thanks Xiaojiang Yu).


Please install: 

- MacOS

```
/Applications/Oasys1.2.app/Contents/MacOS/PythonApp -m pip install https://raw.githubusercontent.com/oasys-kit/shadow3/devel-gfortran-yb66/wheels/shadow3-22.2.7-cp37-cp37m-macosx_10_6_intel.whl
```