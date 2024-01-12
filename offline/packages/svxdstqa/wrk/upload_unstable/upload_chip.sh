#!/bin/bash

for i in /direct/phenix+hhj2/dcm07e/vtx/deadmaps_Run14HeAu200/runDiffMaps/chip*
do
#   echo "root -b -q ./testWriteChip.C\(\"$i\",${i:(-10):6}\)"
  root -b -q ./testWriteChip.C\(\"$i\",${i:(-10):6}\)
done

