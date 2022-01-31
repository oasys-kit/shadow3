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
python setup.py sdist build
cp build/lib.linux-x86_64-3.7/Shadow/ShadowLib.cpython-37m-x86_64-linux-gnu.so Shadow/
export LD_LIBRARY_PATH=/users/srio/OASYS1.2/shadow3
