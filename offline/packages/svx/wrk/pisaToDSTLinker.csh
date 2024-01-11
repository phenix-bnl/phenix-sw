#! /bin/csh -f

echo ""
echo "pisaToDSTLinker setup for run11 simulations"

# configuration files
echo "private linker"
#ln -sf /common/s2/Subsystems/hachiya/source/src/wrk/simulation/pisaToDSTLinker.csh .

# rich cabling
ln -sf /afs/rhic.bnl.gov/phenix/software/simulation/head/crk_cabling_vrdc.txt .

# Dch calibrations for MC
ln -sf /afs/rhic.bnl.gov/phenix/software/calibration/sim00/DchCalibration.Mc .

# central arm integrated field map
ln -sf /afs/rhic.bnl.gov/phenix/software/calibration/run2004/fieldIntegral++.dat.run04 fieldIntegral.dat

# 3D magnetic field map
ln -sf /afs/rhic.bnl.gov/phenix/PHENIX_LIB/simulation/Sim3D++.root .

# muon arm material map
# it is unchanged since Run07
ln -sf /afs/rhic.bnl.gov/phenix/software/calibration/run2010/pisafile.dat.cZ .

# svx pisapar
#ln -sf /common/s2/Subsystems/hachiya/simulation/reco/svxPISA.par .

# root macros
#cp /common/s2/Subsystems/hachiya/simulation/reco/pisaToDST.C .
#cp /common/s2/Subsystems/hachiya/simulation/reco/pisaToDST_IOManager.C .
#cp /common/s2/Subsystems/hachiya/simulation/reco/Fun4All_RecoDST_sim.C .
#chmod +w pisaToDST.C
#chmod +w pisaToDST_IOManager.C
#chmod +w Fun4All_RecoDST_sim.C

# input file
# there is currently no available PISA file for run10

echo ""
echo "setup script completed"
echo "First generate some PISA file using the pisaLinker.csh script"
echo "Then process it by typing root -b -q pisaToDST.C"
echo "Please report any broken link by email to phenix-off-l@lists.bnl.gov"
