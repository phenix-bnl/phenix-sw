#!/bin/csh                                                                      

unsetenv OFFLINE_MAIN
unsetenv ONLINE_MAIN
unsetenv ROOTSYS
source /opt/phenix/bin/phenix_setup.csh ana

cd /direct/phenix+hl/tujuba/emc_calib/iterations

./make_pi0_hist.sh 0

#                                                                               


