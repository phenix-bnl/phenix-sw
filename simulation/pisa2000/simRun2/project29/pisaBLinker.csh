#! /bin/csh -f
#
# DO NOT CHANGE THE FIRST LINE ABOVE
# IT SIGNIFIES THAT THIS SCRIPT IS A C-SHELL SCRIPT
# THE -f INDICATES THAT THE USER'S .cshrc SCRIPT IS NOT RE-READ BY THIS SCRIPT
#
echo " "
echo " pisaBLinker full field, normal arms Year 2 setup script initiated: version Jun 20, 2002 (ROOT3.01)"
echo " "

setenv ROOTSYS /opt/phenix/root-3.01.05
setenv PATH $ROOTSYS/bin:/afs/rhic/phenix/PHENIX_LIB/simulation/run2b/newJun01/bin:$PATH
setenv LD_LIBRARY_PATH .:$ROOTSYS/lib:/afs/rhic/phenix/PHENIX_LIB/simulation/run2b/newJun20/lib:$LD_LIBRARY_PATH

ln -s /afs/rhic/phenix/PHENIX_LIB/simulation/run2a/map_both_february_97.dat .
ln -s /afs/rhic/phenix/PHENIX_LIB/simulation/run2b/phnx.par .
ln -s /afs/rhic/phenix/PHENIX_LIB/simulation/run2a/gffgo.dat .
ln -s /afs/rhic/phenix/PHENIX_LIB/simulation/run2a/flukaaf.dat .
ln -s /afs/rhic/phenix/PHENIX_LIB/simulation/run2a/glogon.kumac .
ln -s /afs/rhic/phenix/PHENIX_LIB/simulation/run2a/xsneut95.dat .

cp /afs/rhic/phenix/PHENIX_LIB/simulation/run2b/pisa.kumac .
chmod +w pisa.kumac
cp /afs/rhic/phenix/PHENIX_LIB/simulation/run2b/pisa.input .
chmod +w pisa.input
cp /afs/rhic/phenix/PHENIX_LIB/simulation/run2a/event.par .
chmod +w event.par

ln -s /phenix/data11/rhphemds/run2/hijing/hji135evt_20000auauminb200sq01_060602.dat .

echo " "
echo " pisaBLinker full field, Run 2 setup script completed "
echo " "
echo " pisa.kumac file has Muon Arms installed"
echo " phnx.par file has PC2/PC3 installed in the West Arm "
echo " event.par file has +/- 22 cm sigma Gaussian Z distribution "
echo " You may need to edit the pisa.kumac, the pisa.input, or the event.par files "
echo " "
echo " Type  pisa < pisa.input >& pisa.out & " to track one HIJING event [2-3 CPU minutes]
echo " "
