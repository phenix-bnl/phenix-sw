#! /bin/tcsh
set dir = $1
source /opt/phenix/bin/phenix_setup.csh
foreach file ($dir/*.proot)
set fname=`echo $file | sed "s/\(\/.*\/\)/ /g"`
root -b -q dstlistgenerator.C\(\"$dir\",\"$fname\"\)
end
