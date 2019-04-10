REM  see https://github.com/GeoscienceAustralia/tcrm/wiki/Compiling-the-C-extensions-in-MinGW-on-Windows-platforms-with-Anaconda

REM
REM  python libs
REM
copy c:\anaconda3\python3.dll .
REM  create .def
C:\mingw64\mingw32\bin\gendef python3.dll
dlltool --dllname python3.dll --def python3.def --output-lib libpython3.a

REM
REM  compile bind
REM

gcc -c src/c/shadow_bind_python.c -I./src/def/ -Ic:/anaconda3/include -Ic:/anaconda3/lib/site-packages/numpy/core/include/ -o shadow_bind_python.o -Wall -O -std=c99 -D MSWIN64

REM
REM  create binding
REM
gcc -shared -s shadow_bind_python.o -Lc:/anaconda3/libs -L./src -lshadow3c -lshadow3 libpython3.a -o ShadowLib.pyd


REM  install
copy ShadowLib.pyd Shadow
copy libshadow3.dll Shadow
copy libshadow3c.dll Shadow

REM
REM  test it
REM

copy tests\test_lens.py .
REM Check that it imports without error:
REM python -c "import Shadow"
python test_lens.py

REM
REM Install
REM
mkdir c:\anaconda3\Lib\site-packages\Shadow
copy Shadow\*.* c:\anaconda3\Lib\site-packages\Shadow
