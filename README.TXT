

=====================================================

          SHADOW 3 development history

=====================================================


This directory contains an incomplete version of SHADOW3

version 3alpha0 (2009-09-25)
============================

  Achieved: 
            Arrange libraries into modules.
            gen_source:  for geometrical sources only. 
            input_source:  for defining source parameters
            translate: for looking at the results

  Files: 

    library modules: The shadow libraries have been converted to f95 modules

      The following modules are independent (no interdependences among them): 

          stringio.f95   :  tools to manipulate strings, and error input-output
          beamio.f95     :  tools to read/write binary files (rbeam, rbeam18, write_off and write_off18) 
          gfile.f95      :  a completely new f95 module to manuipulate gFiles (for start.xx input/output)
          math.f95       :  mathematical routines 
          math_imsl.f95  :  mathematical routines from the imsl library

          Note that each module has also a main program test_<module>.f95 to test it.
           
      Main shadow module: 
          shadow.f95    :  contains the shadow variables, routines that need the variables pool,
                           and routines for the main calculations. To present, only geometrical
                           sources are implemented (routine source1)

      Main programs:
          test_<module>.f95 :  to test individually the modules
          test_integers.f95 :  to test variable types in different machines (for compatibility 32 and 64 bits)
          gen_source.f95    :  to create the source
          input_source.f95  :  prompt interface for creating source
          translate.f95     :  to translate binary files into ASCII

      Others:
          README.TXT          : This file
          module_template.f95 : A template used to write modules
          compile_g95.sh      : Shell script (unix) to compile everything with g95
          compile_intel.sh    : Shell script (unix) to compile everything with intel fortran
          compile.bat         : Shell script (dos) to compile everything
          clean.sh            : Shell script (unix) to clean files

  Test: 
          ./input_source
           Write default start.00 file? [Y/N]: 1
           Exit from INPUT_SOURCE
          
          ./gen_source start.00
           Generated  250  rays out of  30000
                      500
                      750
                      1000
                      1250
                      ...
                      29500
                      29750
                      30000
           Exit from SOURCE
  
           ./translate
            File for input ? begin.dat
            Read          30000  rays.
            Each ray has  13  entries.
            Enter filename or - for standard output.
            Output filename or device ? -
            How many rays to translate ? 10
            Create comma separated values (CSV) output [y/N] ? 1
            0.90425965585623E-03, 0.48594460650556-269,-0.23265626663830E-02, 0.19995073275591E-06, 0.99999999999746
            ,-0.22468816931490E-05, 0.99999999999998    ,-0.19995073275541E-06, 0.44926564096099E-12,  1.0000000000000    ,
            50676899.194623    ,  1.0000000000000    , 0.0000000000000
            -0.63221982757456E-04, 0.48594460650556-269, 0.77170632744916E-03, 0.16110191424301E-06, 0.99999999999519    ,
            0.30984191387070E-05, 0.99999999999999    ,-0.16110191424224E-06,-0.49916125437288E-12,  1.0000000000000    ,
            50676899.194623    ,  2.0000000000000    , 0.0000000000000
            ...


Technical notes
===============
    1) No makefiles, no include files, no common blocks, no C code, no N_DIM (works for ANY number of rays)
    2) math-wran: calls the native f95 random number generator. Note that the sequence is different than for shadow2.x
    3) platforma+compilers. Tested on:
          linux (red hat) boxes 32 and 64 bits using G95 (GCC 4.0.3 (g95 0.91!) Jun  5 2007 
          linux (red hat) 64 bits using ifort (IFORT) 10.0 20070426 Intel Corporation  (note that *.f95 are not accepted, copied to *.f90)
          Windows Vista 32 bits using G95 (GCC 4.1.2 (g95 0.92!) May 31 2009
          MacOS Darwin Kernel Version 8.11.1 G95 (GCC 4.0.3 (g95 0.91!) Aug 10 2007)
 
          Failed to compile shadow.f95 with gfortran on Darwin...
               

