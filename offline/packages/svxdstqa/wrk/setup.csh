#!/usr/local/bin/tcsh -f
setenv HOME /home/phnxvtx/hachiya
setenv prompt 1
source /etc/csh.login
foreach i (/etc/profile.d/*.csh)
  source $i
end
source $HOME/.login
unsetenv OFFLINE_MAIN
unsetenv ONLINE_MAIN
unsetenv ROOTSYS

source /opt/phenix/bin/phenix_setup.csh ana.241
setenv LD_LIBRARY_PATH /common/s2/Subsystems/hachiya/install/lib:$LD_LIBRARY_PATH

