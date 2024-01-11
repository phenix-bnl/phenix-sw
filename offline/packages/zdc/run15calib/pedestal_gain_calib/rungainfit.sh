#!/bin/bash

root -l -q 'fitGainSMD.C(423451, 0)'
root -l -q 'fitGainSMD.C(423451, 1)'
root -l -q 'fitGainSMD.C(423451, 2)'
root -l -q 'fitGainSMD.C(423451, 3)'
root -l -q 'fitGainSMD.C(423451, 4)'
root -l -q 'fitGainSMD.C(423451, 5)'
root -l -q 'fitGainSMD.C(423451, 6)'
root -l -q 'fitGainSMD.C(423451, 7)'
root -l -q 'fitGainSMD.C(423451, 8)'
root -l -q 'fitGainSMD.C(423451, 9)'
root -l -q 'fitGainSMD.C(423451, 10)'
root -l -q 'fitGainSMD.C(423451, 11)'
root -l -q 'fitGainSMD.C(423451, 12)'
root -l -q 'fitGainSMD.C(423451, 13)'
root -l -q 'fitGainSMD.C(423451, 14)'
root -l -q 'fitGainSMD.C(423451, 15)'
root -l -q 'fitGainSMD.C(423451, 16)'
root -l -q 'fitGainSMD.C(423451, 17)'
root -l -q 'fitGainSMD.C(423451, 18)'
root -l -q 'fitGainSMD.C(423451, 19)'
root -l -q 'fitGainSMD.C(423451, 20)'
root -l -q 'fitGainSMD.C(423451, 21)'
root -l -q 'fitGainSMD.C(423451, 22)'
root -l -q 'fitGainSMD.C(423451, 23)'
root -l -q 'fitGainSMD.C(423451, 24)'
root -l -q 'fitGainSMD.C(423451, 25)'
root -l -q 'fitGainSMD.C(423451, 26)'
root -l -q 'fitGainSMD.C(423451, 27)'
root -l -q 'fitGainSMD.C(423451, 28)'
root -l -q 'fitGainSMD.C(423451, 29)'
root -l -q 'fitGainSMD.C(423451, 30)'
root -l -q 'fitGainSMD.C(423451, 31)'


cat parameters/gain_s* >parameters/gain.txt
root -l getCorr.C

exit