#
# unix script to compile shadow3alpha0
#

./clean.sh

cp stringio.f95 stringio.f90
cp gfile.f95 gfile.f90
cp  beamio.f95  beamio.f90
cp math.f95 math.f90
cp math_imsl.f95  math_imsl.f90
cp shadow.f95 shadow.f90
cp input_source.f95 input_source.f90
cp gen_source.f95 gen_source.f90
cp translate.f95 translate.f90

#
# compile modules
#
ifort -c stringio.f90
ifort -c gfile.f90
ifort -c beamio.f90
ifort -c math.f90
ifort -c math_imsl.f90
ifort -c shadow.f90

#
# compile and link main programs
#
ifort -c gen_source.f90
ifort -o gen_source gen_source.o shadow.o gfile.o beamio.o stringio.o math.o math_imsl.o 

ifort -c input_source.f90
ifort -o input_source  input_source.o  stringio.o

ifort -c translate.f90
ifort -o translate  translate.o  stringio.o beamio.o


#
# compile and link test programs
#

# ifort -c test_integers.f90
# ifort -o test_integers test_integers.o 
# 
# ifort -c test_stringio.f90
# ifort -o test_stringio stringio.o test_stringio.o
# 
# ifort -c test_gfile.f90
# ifort -o test_gfile gfile.o test_gfile.o
# 
# ifort -c test_beamio.f90
# ifort -o test_beamio beamio.o test_beamio.o
# 
# ifort -c test_math.f90
# ifort -o test_math math.o test_math.o
# 
# ifort -c test_math_imsl.f90
# ifort -o test_math_imsl math_imsl.o test_math_imsl.o
# 
# ifort -c test_shadow.f90
# ifort -o test_shadow shadow.o gfile.o beamio.o stringio.o math.o math_imsl.o test_shadow.o 

