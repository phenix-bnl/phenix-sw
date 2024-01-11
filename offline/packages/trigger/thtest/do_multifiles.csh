#!/bin/csh -f
##############################################

setenv LD_LIBRARY_PATH ./install/lib:$LD_LIBRARY_PATH
setenv OO_FD_BOOT $PHENIX_FD_BOOT

root -b -q -x run_multifiles.C\(\"files_to_test_oldpp.list\"\) | tee  log_thtest.txt
root -b -q -x run_multifiles.C\(\"files_to_test.list\"\) | tee -a log_thtest.txt
grep ___thtest__ log_thtest.txt > thtest_output.txt
