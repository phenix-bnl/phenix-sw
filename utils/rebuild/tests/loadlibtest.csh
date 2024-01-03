#!/bin/csh
# boost libs and libs requiring an external (pmonitor->pinit(), pdst->dinit())
# are excluded
foreach i ( $OFFLINE_MAIN/lib/*.so )
switch($i)
  case $OFFLINE_MAIN/lib/libpmon*:
  case $OFFLINE_MAIN/lib/libboost*:
  case $OFFLINE_MAIN/lib/libpdst*:
    breaksw
  default:
    root.exe -b -q loadlibtest.C\(\"$i\"\)
  breaksw
  endsw
end
