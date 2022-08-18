rm -rf build dist
cp src/c/shadow_bind_python.c  .
cp src/c/shadow_bind_python.h  .
cp src/c/shadow_bind_c.h .
cp src/def/shadow_source.def .
cp src/def/shadow_oe.def .
cd src
make 
make lib
cd ..
cp src/lib* .
cp Shadow/__init__.py .
# python setup.py sdist build
/Applications/Oasys1.2.app/Contents/MacOS/PythonApp  setup.py sdist build
/Applications/Oasys1.2.app/Contents/MacOS/PythonApp  setup.py bdist_wheel
#cp build/lib.macosx-10.6-intel-3.7/Shadow/ShadowLib.cpython-37m-darwin.so Shadow/
cp build/lib/Shadow/ShadowLib.cpython-37m-darwin.so Shadow/
export LD_LIBRARY_PATH=/users/srio/OASYS1.2/shadow3
