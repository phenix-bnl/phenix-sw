#!/bin/bash

for i in /direct/phenix+hhj2/dcm07e/vtx/deadmaps_Run14HeAu200/runDiffMaps/pixel*
do
#   echo $i
#   echo "root -b -q ./testWriteDiff.C\(\"$i\",${i:(-10):6}\)"
  root -b -q ./testWriteDiff.C\(\"$i\",${i:(-10):6}\)
done
