#
# this script creates shadow_version.h
#
/bin/rm -f version.txt
echo '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++' > version.txt
echo '                           compilation settings                    ' >> version.txt
echo '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++' >> version.txt
echo ' ' >> version.txt
echo ' +++++date (date):' >> version.txt
date >> version.txt
echo ' ' >> version.txt
echo ' +++++Compiler: '  >> version.txt
echo $1 '--version:' >> version.txt
$1 --version  >> version.txt
echo ' ' >> version.txt
echo ' +++++using Unix platform (uname -a): ' >> version.txt
uname -a >> version.txt
echo ' ' >> version.txt
echo ' +++++building sequence (make -n):' >> version.txt
make -n >> version.txt
echo '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++' >> version.txt


sed -e 's/^/print *,"/' < version.txt > tmp1.h
sed -e 's/$/"/' < tmp1.h > fortran/shadow_version.h
 
/bin/rm version.txt tmp1.h
