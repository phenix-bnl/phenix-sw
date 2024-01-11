#!/bin/bash

runnumber=$1
echo "run$runnumber Fit SMD Pedestal"
root -b -l -q "fitPed.C($runnumber,0)"
root -b -l -q "fitPed.C($runnumber,1)"
root -b -l -q "fitPed.C($runnumber,2)"
root -b -l -q "fitPed.C($runnumber,3)"
root -b -l -q "fitPed.C($runnumber,4)"
root -b -l -q "fitPed.C($runnumber,5)"
root -b -l -q "fitPed.C($runnumber,6)"
root -b -l -q "fitPed.C($runnumber,7)"
root -b -l -q "fitPed.C($runnumber,8)"
root -b -l -q "fitPed.C($runnumber,9)"
root -b -l -q "fitPed.C($runnumber,10)"
root -b -l -q "fitPed.C($runnumber,11)"
root -b -l -q "fitPed.C($runnumber,12)"
root -b -l -q "fitPed.C($runnumber,13)"
root -b -l -q "fitPed.C($runnumber,14)"
root -b -l -q "fitPed.C($runnumber,15)"
root -b -l -q "fitPed.C($runnumber,16)"
root -b -l -q "fitPed.C($runnumber,17)"
root -b -l -q "fitPed.C($runnumber,18)"
root -b -l -q "fitPed.C($runnumber,19)"
root -b -l -q "fitPed.C($runnumber,20)"
root -b -l -q "fitPed.C($runnumber,21)"
root -b -l -q "fitPed.C($runnumber,22)"
root -b -l -q "fitPed.C($runnumber,23)"
root -b -l -q "fitPed.C($runnumber,24)"
root -b -l -q "fitPed.C($runnumber,25)"
root -b -l -q "fitPed.C($runnumber,26)"
root -b -l -q "fitPed.C($runnumber,27)"
root -b -l -q "fitPed.C($runnumber,28)"
root -b -l -q "fitPed.C($runnumber,29)"
root -b -l -q "fitPed.C($runnumber,30)"
root -b -l -q "fitPed.C($runnumber,31)"

cd parameters

cat pedestal_s* >pedestal.txt

cd ../pedestalfile

head -8 ZdcCalib.pedestal.default.run423451 > newpedestal.txt
cat ../parameters/pedestal.txt >> newpedestal.txt

mv newpedestal.txt ZdcCalib.pedestal.run$runnumber

cd ..

exit