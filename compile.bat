rem #
rem # dos script to compile shadow3alpha0
rem #

del *.o  *.mod *.exe

rem #
rem # compile modules
rem #
g95 -c stringio.f95
g95 -c gfile.f95
g95 -c beamio.f95
g95 -c math.f95
g95 -c math_imsl.f95
g95 -c shadow.f95

rem #
rem # compile and link main programs
rem #
g95 -c gen_source.f95
g95 -o gen_source gen_source.o shadow.o gfile.o beamio.o stringio.o math.o math_imsl.o 

g95 -c input_source.f95
g95 -o input_source  input_source.o  stringio.o

g95 -c translate.f95
g95 -o translate  translate.o  stringio.o beamio.o


rem #
rem # compile and link test programs
rem #

rem # g95 -c test_integers.f95
rem # g95 -o test_integers test_integers.o 
rem # 
rem # g95 -c test_stringio.f95
rem # g95 -o test_stringio stringio.o test_stringio.o
rem # 
rem # g95 -c test_gfile.f95
rem # g95 -o test_gfile gfile.o test_gfile.o
rem # 
rem # g95 -c test_beamio.f95
rem # g95 -o test_beamio beamio.o test_beamio.o
rem # 
rem # g95 -c test_math.f95
rem # g95 -o test_math math.o test_math.o
rem # 
rem # g95 -c test_math_imsl.f95
rem # g95 -o test_math_imsl math_imsl.o test_math_imsl.o
rem # 
rem # g95 -c test_shadow.f95
rem # g95 -o test_shadow shadow.o gfile.o beamio.o stringio.o math.o math_imsl.o test_shadow.o 

