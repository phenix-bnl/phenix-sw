#! /bin/csh -f
#
# DO NOT CHANGE THE FIRST LINE ABOVE
# IT SIGNIFIES THAT THIS SCRIPT IS A C-SHELL SCRIPT
# THE -f INDICATES THAT THE USER'S .cshrc SCRIPT IS NOT RE-READ BY THIS SCRIPT
#
echo " "
echo " taskBLinker Year2 PRDF-to-DST script initiated: version June 20, 2002"
echo " "

setenv ROOTSYS /opt/phenix/root-3.01.05
setenv PATH $ROOTSYS/bin:/afs/rhic/phenix/PHENIX_LIB/simulation/run2b/newMay30/bin:$PATH
setenv LD_LIBRARY_PATH .:$ROOTSYS/lib:/afs/rhic/phenix/PHENIX_LIB/simulation/run2b/newJun20/lib:$LD_LIBRARY_PATH

ln -s /phenix/data11/rhphemds/run00/muon500meV_20th85ph175_120300z00.root .
ln -s /phenix/data11/rhphemds/run00/muon500meV_20th85ph175_120300z00.root PISA2000Normal.root
ln -s /phenix/data11/rhphemds/run2/macroTest/PISAEvent.piminus5000-1.5.root .
ln -s /phenix/data11/rhphemds/run2/macroTest/muon500meV_20th85ph003_043001z00.root .
ln -s /phenix/data11/rhphemds/run2/macroTest/pos1GeV_20th85ph003_043001z00.root .
ln -s /phenix/data11/rhphemds/run00/positron1gev_20th85ph175_120400z00.root .
ln -s /phenix/data11/rhphemds/run00/hji135_20auau0b20b130sq01_120400.root .
ln -s /phenix/data11/rhphemds/run2/macroTest/muon5GeV_1000th165ph10_050901.root .
ln -s /phenix/data11/rhphemds/run2/macroTest/hji135evt_20auauminb200sq02_053002z10.root .

/afs/rhic/phenix/software/calibration/data/LuxorLinker.pl -1 33694
#
# Special TOF ASCII files which are not picked up by LuxorLinker.pl but were picked up by LuxorLinker.csh
#
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/toffemmap.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofslatoffset.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofPedestal.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofTvcConv.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofQvcConv.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofSlewPar.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofToffset.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofYoffset.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofVelocity.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofElossConv.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofGlobalT.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofElossConv.txt.geant-year2 .
#
# New version of tofpanelgeo.txt May 17, 2002 (from Akio Kiyomichi)
#
ln -s /afs/rhic/phenix/software/simulation/run2b/tofpanelgeo.txt.year1 tofpanelgeo.txt

ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/toffemmap.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofslatoffset.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofPedestal.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofTvcConv.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofQvcConv.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofSlewPar.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofToffset.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofYoffset.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofVelocity.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofElossConv.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofGlobalT.txt.geant .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofpanelgeo.txt.geant .

ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofcablemap.txt .

ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/crk_slew.txt .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/crk_tac.txt .

#
# Versions from Tatsuya on June 14, 2002
#
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/toffemmap.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofpanelgeo.txt.year2
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofslatoffset.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofPedestal.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofTvcConv.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofQvcConv.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofSlewPar.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofToffset.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofYoffset.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofVelocity.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofElossConv.txt.year2 .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/tofGlobalT.txt.year2 .

#
# BBC links as of May 9, 2002 for Au+Au
#
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.adc .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.fakePedestal .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.overflow0 .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.overflow1 .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.pedestal .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.pmtgain .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.slewpar0 .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.slewpar1 .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.tdc0 .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.tdc1 .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.threshold .
ln -s /afs/rhic/phenix/software/simulation/run2b/29255.timereso .

#
# BBC links as of May 9, 2002 for p+p 
#
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.adc .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.fakePedestal .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.overflow0 .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.overflow1 .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.pedestal .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.pmtgain .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.slewpar0 .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.slewpar1 .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.tdc0 .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.tdc1 .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.threshold .
ln -s /afs/rhic/phenix/software/simulation/run2b/37583.timereso .

#
# padGeometry.txt file from LuxorLinker is not useful here (Paul Nilsson)
# crk_adc.txt file from LuxorLinker is not useful here (Takashi Hachiya)
#
rm padGeometry.txt

echo " "
echo " Using Run2, GEANT version of padGeometry.txt file"
echo " Using modified (December 22, 2000) version of crk_adc.txt file"
echo " "
ln -s /afs/rhic/phenix/software/simulation/run2b/padGeometrySimulationRun2Z0Fix.txt padGeometry.txt
ln -s /afs/rhic/phenix/software/simulation/run2a/MUTcalib.Year2.0.dat .
ln -s /afs/rhic/phenix/software/simulation/run2a/mui-fem-config.dat .
ln -s /afs/rhic/phenix/software/simulation/run2a/mui-panel-geom.dat .
ln -s /afs/rhic/phenix/software/simulation/run2a/mui-panel-size.dat .
ln -s /afs/rhic/phenix/software/simulation/run2a/mui-tube-geom.dat .
ln -s /afs/rhic/phenix/software/simulation/run2a/mui-tube-size.dat .
ln -s /afs/rhic/phenix/software/simulation/run2a/DchEfficiencyYear2.Mc .
cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDST.C .
cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDST.input .
cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDSTPosEast.input .
cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDSTPosWest.input .
cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDSTHijing.input .
#cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDSTMuSouth.input .
#cp /afs/rhic/phenix/software/simulation/run2b/PISAtoDSTMerge.input .

#ln -s /afs/rhic/phenix/software/simulation/run00a/crkFiles/crk_cabling_vrdc.txt .
ln -s /afs/rhic/phenix/software/simulation/run00a/crkFiles/crk_adc.txt .

ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/run2000/MomRec2DTableELECTRON_HIRES_DATA.root .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/run2000/MomRec2DTableKMINUS_HIRES_DATA.root .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/run2000/MomRec2DTablePBAR_HIRES_DATA.root .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/run2000/MomRec2DTablePION_HIRES .
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/run2000/MomRec2DTablePION_HIRES_DATA.root .

# from Xie Wei July 16, 2002
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/run2002/ertEMCalGainVarRUN2.dat .
# Added by H.T. Dec.24, 2002
ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/SM_Word_map.dat .


chmod +w *C
chmod +w PISAtoDST.input

touch core
chmod -w core

echo " "
echo " taskBLinker full 2D field, normal arms setup script completed "
echo " "
echo " Type    root -b < PISAtoDST.input >& task.out &   20 event PISA-to-DST task (muons East)"
echo " Type    root -b < PISAtoDSTPosEast.input >& task.out &   20 event PISA-to-DST task (positrons East)"
echo " Type    root -b < PISAtoDSTPosWest.input >& task.out &   20 event PISA-to-DST task (positrons West)"
echo " Type    root -b < PISAtoDSTHijing.input >& task.out &    launch a 20 event PISA-to-DST task (HIJING min bias, Z0 = 10)"
#echo " Type    root -b < taskMuSouth.input >& task.out &   20 event PISA-to-DST task (5 GeV Muons South)"
#echo " Type    root -b < taskMerge.input >& task.out &    20 event PISA-to-DST task (Pi- Merge 4 files)"
echo " "
