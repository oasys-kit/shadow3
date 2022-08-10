Wheels for testing shadow3 
==========================

This directory contains commands for installing shadow3 v 22.8.10 for testing before release.

They have been built using the branch: 
https://github.com/oasys-kit/shadow3/tree/devel-gfortran-yb66


It included changes to compile with new gfortran compilers (thanks Tom Schoonjans)
and calculation of any crystal structure via a new bragg preprocessor file (thanks Xiaojiang Yu).


Please install: 

- MacOS 

```
/Applications/Oasys1.2.app/Contents/MacOS/PythonApp -m pip install https://silx.gitlab-pages.esrf.fr/bob/shadow3/shadow3-22.8.10-cp37-cp37m-macosx_10_9_x86_64.whl
```

- Linux
```
pip install https://silx.gitlab-pages.esrf.fr/bob/shadow3/shadow3-22.8.10-cp37-cp37m-manylinux_2_17_x86_64.manylinux2014_x86_64.whl
```

- Windows
```
pip install https://raw.githubusercontent.com/oasys-kit/shadow3/devel-gfortran-yb66/wheels/shadow3-22.8.10-cp37-cp37m-win_amd64.whl
```
