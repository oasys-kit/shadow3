#/bin/bash
echo "**********************"
echo "Chapter 2, Section 4"
echo "**********************"
../shadow3 < primer_ch24_run1.inp
../shadow3 < primer_ch24_mirinfo.inp
../shadow3 < primer_ch24_sysinfo.inp
../shadow3 < primer_ch25_run1.inp
../shadow3 < primer_ch26_run1.inp

echo "**********************"
echo "Chapter 6, Section 1"
echo "**********************"
../shadow3 < primer_ch61_run1.inp
../shadow3 < primer_ch61_contour.inp
../shadow3 < primer_ch61_pixelized.inp


echo "**********************"
echo "Chapter 6, Section 2"
echo "**********************"
../shadow3 < primer_ch62_run1.inp
../shadow3 < primer_ch62_recolor.inp
../shadow3 < primer_ch62_run2.inp

echo "**********************"
echo "Chapter 6, Section 3"
echo "**********************"
../shadow3 < primer_ch63_run1.inp
../shadow3 < primer_ch63_contour.inp

../shadow3 < primer_ch63_run2.inp
../shadow3 < primer_ch63_results2.inp

echo "**********************"
echo "Chapter 6, Section 4"
echo "**********************"
../shadow3 < primer_ch64_bragg.inp
../shadow3 < primer_ch64_source.inp
../shadow3 < primer_ch64_run1.inp
../shadow3 < primer_ch64_results1.inp
../shadow3 < primer_ch64_run2.inp
../shadow3 < primer_ch64_results2.inp

echo "**********************"
echo "Chapter 6, Section 5"
echo "**********************"
../shadow3 < primer_ch65_source.inp
../shadow3 < primer_ch64_run1.inp
../shadow3 < primer_ch65_run2.inp
../shadow3 < primer_ch64_results2.inp

echo "**********************"
echo "      E n d           "
echo "**********************"


echo "**********************"
echo "      CLEANING        "
echo "**********************"

shopt -s extglob
/bin/rm !(README.txt|primer.sh|*.inp)


