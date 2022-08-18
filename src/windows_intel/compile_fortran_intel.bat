del \x64\Debug\*.*

@REM REM
@REM REM  compile shadow sources (it does not include fortran binding)
@REM REM
@REM ifort /nologo /debug:full /Od /fpp
@REM /I"C:\Users\srio\shadow3VS\test1\test1\\..\..\src\fortran"
@REM /I"C:\Users\srio\shadow3VS\test1\test1\\..\..\src\def"
@REM /I"C:\Users\srio\shadow3VS\test1\test1\\..\..\src\c"
@REM /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\" /Fd"x64\Debug\vc170.pdb" /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c /Qlocation,link,"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.31.31103\bin\HostX64\x64" /Qm64
@REM "C:\Users\srio\shadow3VS\src\fortran\shadow_version.f90"


set PATH=%PATH%;C:\"Program Files (x86)"\Intel\oneAPI\mpi\latest\env\..\libfabric\bin\utils;C:\"Program Files (x86)"\Intel\oneAPI\mpi\latest\env\..\libfabric\bin;C:\"Program Files (x86)"\Intel\oneAPI\mpi\latest\env\..\bin\release;C:\"Program Files (x86)"\Intel\oneAPI\mpi\latest\env\..\bin;C:\"Program Files (x86)"\Intel\oneAPI\debugger\latest\env\\..\gdb\intel64\bin;C:\"Program Files (x86)"\Intel\oneAPI\compiler\latest\windows\bin;C:\"Program Files (x86)"\Intel\oneAPI\compiler\latest\windows\lib;C:\"Program Files (x86)"\Intel\oneAPI\compiler\latest\windows\bin\intel64;C:\"Program Files (x86)"\Intel\oneAPI\compiler\latest\windows\redist\ia32_win\compiler

@REM srio: Removed /Qm64 /Qlocation,link,"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.31.31103\bin\HostX64\x64"  /I"..\c"  /I"..\fortran" /Fd"x64\Debug\vc170.pdb"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_version.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_globaldefinitions.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\stringio.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\gfile.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_beamio.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_math.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_variables.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_roughness.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_kernel.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_synchrotron.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_pre_sync.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_pre_sync_urgent.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_preprocessors.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_postprocessors.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_crl.f90"
ifort -I /nologo /debug:full /Od /fpp   /I"..\def"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\"  /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c "..\fortran\shadow_bind_f.f90"



@REM
@REM  create library
@REM
Lib /OUT:"x64\Debug\ShadowLib.lib" /NOLOGO "x64\Debug\shadow_version.obj" "x64\Debug\shadow_globaldefinitions.obj" "x64\Debug\shadow_math.obj" "x64\Debug\stringio.obj" "x64\Debug\shadow_preprocessors.obj" "x64\Debug\shadow_pre_sync_urgent.obj" "x64\Debug\shadow_beamio.obj" "x64\Debug\gfile.obj" "x64\Debug\shadow_variables.obj" "x64\Debug\shadow_roughness.obj" "x64\Debug\shadow_postprocessors.obj" "x64\Debug\shadow_kernel.obj" "x64\Debug\shadow_synchrotron.obj" "x64\Debug\shadow_pre_sync.obj" "x64\Debug\shadow_crl.obj" "x64\Debug\shadow_bind_f.obj"


@REM
@REM  create shadow3.exe
@REM

ifort -I /nologo /debug:full /Od /fpp  /I"..\fortran" /I"..\def" /I"..\c"  /D_COMPILE4WIN /free /warn:interfaces /module:"x64\Debug\\" /object:"x64\Debug\\" /Fd"x64\Debug\vc170.pdb" /traceback /check:bounds /check:stack /libs:static /threads /dbglibs /c /Qlocation,link,"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.31.31103\bin\HostX64\x64" "..\fortran\shadow3.f90"
Link /OUT:"shadow3.exe" /INCREMENTAL:NO /NOLOGO /MANIFEST /MANIFESTFILE:"x64\Debug\shadow3.exe.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /DEBUG /PDB:"C:\Users\srio\shadow3VS\test1\test1shadow3exe\x64\Debug\shadow3.pdb" /SUBSYSTEM:CONSOLE /IMPLIB:"C:\Users\srio\shadow3VS\test1\test1shadow3exe\x64\Debug\shadow3.lib" "x64\Debug\shadow_version.obj" "x64\Debug\shadow_globaldefinitions.obj" "x64\Debug\shadow_pre_sync_urgent.obj" "x64\Debug\shadow_math.obj" "x64\Debug\stringio.obj" "x64\Debug\shadow_beamio.obj" "x64\Debug\shadow_preprocessors.obj" "x64\Debug\gfile.obj" "x64\Debug\shadow_roughness.obj" "x64\Debug\shadow_variables.obj" "x64\Debug\shadow_postprocessors.obj" "x64\Debug\shadow_kernel.obj" "x64\Debug\shadow_crl.obj" "x64\Debug\shadow_synchrotron.obj" "x64\Debug\shadow_pre_sync.obj" "x64\Debug\shadow_bind_f.obj" "x64\Debug\shadow3.obj"



@REM REM
@REM REM  copy created binary and libraries to main dir
@REM REM
copy x64\Debug\ShadowLib.lib ..\..
copy shadow3.exe ..\..
