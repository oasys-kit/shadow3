import os
import zipfile
import platform

clean_temporary_files = False

if platform.system() != 'Windows':
    print("This scripts is only for Windows. Aborting...")
    exit()

# compilation
os.system("cd src\windows_gfortran & compile_gfortran.bat")

# clean and create the new destination directory dist_with_dlls
try:
    os.system("del /Q .\dist_with_dlls\*.*")
except:
    pass
try:
    os.system("rmdir dist_with_dlls")
except:
    pass

os.system("mkdir dist_with_dlls")

# copy other dlls (from mingw64) to main directory
os.system("copy .\dlls\libgcc_s_sjlj-1.dll .")
os.system("copy .\dlls\libquadmath-0.dll .")


# main loop over the wheels dound in .\dist
wheels = os.listdir(".\dist")

for wheel in wheels:
    filename, file_extension = os.path.splitext(wheel)
    # print(">>>", filename, file_extension)
    if file_extension == '.whl':
        filezip = ".\dist\%s.zip" % filename
        os.system("copy .\dist\%s.whl %s" % (filename, filezip))

        for source_path in ['libshadow3.dll','libshadow3c.dll','libgcc_s_sjlj-1.dll','libquadmath-0.dll']:
            with zipfile.ZipFile(filezip, 'a') as zipf:
                # Add a file located at the source_path to the destination within the zip
                # file. It will overwrite existing files if the names collide, but it
                # will give a warning
                destination = 'Shadow\%s' % source_path
                zipf.write(source_path, destination)
                print("file %s added to (%s) %s" % (source_path, filezip, destination))

        os.system("copy .\dist\%s.zip .\dist_with_dlls\%s.whl" % (filename, filename))

        if clean_temporary_files:
            os.system("del %s" % (filezip))


if clean_temporary_files:
    for source_path in ['libshadow3.dll', 'libshadow3c.dll', 'libgcc_s_sjlj-1.dll', 'libquadmath-0.dll']:
        os.system("del %s" % (source_path))

print("Wheels with dlls are in directory .\dist_with_dlls")