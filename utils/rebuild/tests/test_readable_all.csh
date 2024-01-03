#!/bin/csh
touch notthis.files
foreach i ( /phenix/data14/phnxreco/TestBuild/dsts/* )
echo $i
grep $i notthis.files >& /dev/null
if ($? == 1) then
set dirnam=`basename $i` 
if ( ! -d $dirnam ) then
 mkdir $dirnam
endif
cd $dirnam
root.exe -b -q ../Readability_Test.C\(10,\"$i\"\)
cd ..
else
echo "found file in bad list"
endif
end
