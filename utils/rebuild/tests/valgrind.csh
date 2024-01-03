#! /bin/csh
unsetenv OFFLINE_MAIN
unsetenv ONLINE_MAIN
unsetenv ROOTSYS
source /opt/phenix/bin/phenix_setup.csh new
set macro=$1
set macrobase=`basename $macro .C`
echo $macrobase
set valgrindlogdir="valgrindlog"
if ( ! -d $valgrindlogdir ) then
  mkdir -p $valgrindlogdir
endif
#printenv
valgrind -v --tool=memcheck --num-callers=20 --leak-check=yes --error-limit=no --log-file=$macrobase.valgrind --suppressions=$ROOTSYS/root.supp --leak-resolution=high  --leak-check=full root.exe -q -b $macro\(100,\"$2\"\) >& $valgrindlogdir/$macrobase.log


