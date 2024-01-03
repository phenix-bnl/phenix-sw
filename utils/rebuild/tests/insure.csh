#! /bin/csh
unsetenv OFFLINE_MAIN
unsetenv ONLINE_MAIN
unsetenv ROOTSYS
source /opt/phenix/bin/phenix_setup.csh new+insure
set macro=$1
set macrobase=`basename $macro .C`
echo $macrobase
set insurelogdir="insurelog"
if ( ! -d $insurelogdir ) then
  mkdir -p $insurelogdir
endif
set gusdir="gus"
if ( ! -d $gusdir ) then
  mkdir -p $gusdir
endif
setenv REPORT $insurelogdir/$macrobase.insure
setenv GUSDIR $gusdir/$macrobase.gus
rm -rf $GUSDIR
mkdir $GUSDIR
#printenv
root_insure.exe -q -b $macro\(100,\"$2\"\) >& $insurelogdir/$macrobase.log


