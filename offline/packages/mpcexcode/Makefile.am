AUTOMAKE_OPTIONS = foreign

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include

lib_LTLIBRARIES = \
  libmpcex_base.la \
  libmpcex_interface.la \
  libmpcex_modules.la \
  libmpcex_utils.la

noinst_HEADERS = \
  MpcExBaseLinkDef.h \
  MpcExInterfaceLinkDef.h \
  MpcExModulesLinkDef.h 

# please organize the headers in order, so that we 
# can maintain some sanity here.
# the first set of files without "T" or "m" are 
# "base" files that are compiled in mpcex_base.so. 
# The "T" objects are compiled in mpcex_interface.so.
# The "m" objects are compiled in mpcex_modules.so
include_HEADERS = \
  MpcExConstants.h \
  MpcExMapper.h \
  MpcExGeomToKey.h \
  MpcExSpin.h \
  Exogram.h \
  TMpcExHit.h \
  TMpcExHitContainer.h \
  TMpcExHitSet.h \
  TMpcExHitSort.h \
  TMpcExCalib.h \
  TMpcExCalibContainer.h \
  TMpcExShower.h \
  TMpcExShowerContainer.h \
  TMpcExGeaHit.h \
  TMpcExGeaHitContainer.h \
  TMpcExSingleTrackPi0.h \
  TMpcExMiniCluster.h \
  TMpcExMiniClusterV1.h \
  mMpcExCreateNodeTree.h \
  mMpcExLoadCalibrations.h \
  mMpcExApplyCalibrations.h \
  mMpcExShower.h \
  mMpcExShowerCleaner.h \
  MpcExCalibrateShowerEnergy.h \
  mMpcExQualityCut.h \
  mMpcExUnpackPISA.h \
  mMpcExDigitizeHits.h \
  mMpcExSimEventHeader.h \
  mMpcExEmbed.h \
  mMpcExSingleTrackPi0Finder.h \
  mMpcExMiniClusters.h \
  mMpcExEventCut.h \
  MpcExPISAEventHeader.h \
  MpcExPISAEventHeaderv1.h \
  MpcExPISAEventHeaderv2.h \
  MpcExTSpline1.h \
  MpcExReco.h \
  mToHDF5.h \
  mH5ToRoot.h

libmpcex_base_la_SOURCES = \
  MpcExMapper.C \
  MpcExGeomToKey.C \
  Exogram.C \
  MpcExBaseDict.C

libmpcex_base_la_LDFLAGS = \
  $(AM_LDFLAGS) \
  `root-config --libs`

libmpcex_base_la_LIBADD = \
  -lWrappers \
  -lgea \
  -lPgCal \
  -lphflag \
  -lfun4all \
  -lheader \
  -lTMVA

MpcExBaseDict.C: \
  MpcExMapper.h \
  MpcExGeomToKey.h \
  Exogram.h \
  MpcExConstants.h \
  MpcExBaseLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^


libmpcex_interface_la_SOURCES = \
  TMpcExHit.C \
  TMpcExHitContainer.C \
  TMpcExShower.C \
  TMpcExShowerContainer.C \
  TMpcExGeaHit.C \
  TMpcExGeaHitContainer.C \
  TMpcExSingleTrackPi0.C \
  TMpcExMiniCluster.C \
  TMpcExMiniClusterV1.C \
  MpcExPISAEventHeaderv1.C \
  MpcExPISAEventHeaderv2.C \
  MpcExInterfaceDict.C

MpcExInterfaceDict.C: \
  TMpcExGeaHit.h \
  TMpcExGeaHitContainer.h \
  MpcExPISAEventHeader.h \
  MpcExPISAEventHeaderv1.h \
  MpcExPISAEventHeaderv2.h \
  MpcExInterfaceLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

libmpcex_interface_la_LIBADD = \
  -lmpc \
  -lMpcEx \
  libmpcex_base.la

libmpcex_modules_la_SOURCES = \
  mMpcExCreateNodeTree.C \
  mMpcExLoadCalibrations.C \
  mMpcExApplyCalibrations.C \
  mMpcExShower.C \
  mMpcExShowerCleaner.C \
  MpcExTSpline1.C \
  MpcExCalibrateShowerEnergy.C \
  mMpcExQualityCut.C \
  mMpcExUnpackPISA.C \
  mMpcExDigitizeHits.C \
  mMpcExSimEventHeader.C \
  mMpcExEmbed.C \
  mMpcExSingleTrackPi0Finder.C \
  mMpcExMiniClusters.C \
  mMpcExEventCut.C \
  MpcExReco.C \
  mToHDF5.C \
  mH5ToRoot.C \
  MpcExModulesDict.C 

nodist_libmpcex_modules_la_SOURCES = \
  ml_training_data_bdtg_arm0.h \
  ml_training_data_bdtg_arm0_inner.h \
  ml_training_data_bdtg_arm0_outer.h \
  ml_training_data_bdtg_arm0_E33_0.h \
  ml_training_data_bdtg_arm1.h \
  ml_training_data_bdtg_arm1_inner.h \
  ml_training_data_bdtg_arm1_outer.h \
  ml_training_data_bdtg_arm1_E33_0.h \
  ml_training_data_mlp_arm0.h \
  ml_training_data_mlp_arm0_inner.h \
  ml_training_data_mlp_arm0_outer.h \
  ml_training_data_mlp_arm0_E33_0.h \
  ml_training_data_mlp_arm1.h \
  ml_training_data_mlp_arm1_inner.h \
  ml_training_data_mlp_arm1_outer.h \
  ml_training_data_mlp_arm1_E33_0.h 

MpcExModulesDict.C: \
  mMpcExCreateNodeTree.h \
  mMpcExLoadCalibrations.h \
  mMpcExApplyCalibrations.h \
  mMpcExShower.h \
  mMpcExShowerCleaner.h \
  MpcExCalibrateShowerEnergy.h \
  mMpcExQualityCut.h \
  mMpcExUnpackPISA.h \
  mMpcExDigitizeHits.h \
  mMpcExSimEventHeader.h \
  mMpcExEmbed.h \
  mMpcExSingleTrackPi0Finder.h \
  mMpcExMiniClusters.h \
  mMpcExEventCut.h \
  MpcExReco.h \
  mToHDF5.h \
  mH5ToRoot.h \
  MpcExModulesLinkDef.h 
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

libmpcex_modules_la_LIBADD = \
  -L$(OPT_PHENIX)/lib \
  -lgea \
  -lSubsysReco \
  -lmpc \
  -lSpectrum \
  -lTMVA \
  -lhdf5 \
  -lhdf5_hl \
  -lhdf5_cpp \
  -lhdf5_hl_cpp \
  libmpcex_interface.la

libmpcex_utils_la_SOURCES = \
  MpcExSpin.C

libmpcex_utils_la_LIBADD = \
  -luspin

noinst_PROGRAMS = \
  testexternals1 \
  testexternals2 \
  testexternals3 \
  testexternals4

BUILT_SOURCES = \
  testexternals.C \
  ml_training_data_bdtg_arm0.h \
  ml_training_data_bdtg_arm0_inner.h \
  ml_training_data_bdtg_arm0_outer.h \
  ml_training_data_bdtg_arm0_E33_0.h \
  ml_training_data_bdtg_arm1.h \
  ml_training_data_bdtg_arm1_inner.h \
  ml_training_data_bdtg_arm1_outer.h \
  ml_training_data_bdtg_arm1_E33_0.h \
  ml_training_data_mlp_arm0.h \
  ml_training_data_mlp_arm0_inner.h \
  ml_training_data_mlp_arm0_outer.h \
  ml_training_data_mlp_arm0_E33_0.h \
  ml_training_data_mlp_arm1.h \
  ml_training_data_mlp_arm1_inner.h \
  ml_training_data_mlp_arm1_outer.h \
  ml_training_data_mlp_arm1_E33_0.h 

testexternals1_LDADD = \
  libmpcex_base.la

testexternals2_LDADD = \
  libmpcex_interface.la

testexternals3_LDADD = \
  libmpcex_modules.la

testexternals4_LDADD = \
  libmpcex_utils.la

testexternals1_SOURCES = testexternals.C
testexternals2_SOURCES = testexternals.C
testexternals3_SOURCES = testexternals.C
testexternals4_SOURCES = testexternals.C

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit." > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "   return 0;" >> $@
	echo "}" >> $@


ml_training_data_bdtg_arm0.h:\
	TMVARegression_BDTG.weights.arm0.xml 
	cp $< .
	xxd -i $(<F) > temp1
	head -n -1 temp1 > $@
	rm $(<F)
	rm temp1

ml_training_data_bdtg_arm0_inner.h:\
	TMVARegression_BDTG.weights.arm0_inner.xml 
	cp $< .
	xxd -i $(<F) > temp2 
	head -n -1 temp2 > $@
	rm $(<F)
	rm temp2

ml_training_data_bdtg_arm0_outer.h:\
	TMVARegression_BDTG.weights.arm0_outer.xml 
	cp $< .
	xxd -i $(<F) > temp3 
	head -n -1 temp3 > $@
	rm $(<F)
	rm temp3

ml_training_data_bdtg_arm0_E33_0.h:\
	TMVARegression_BDTG.weights.arm0_E33_0.xml 
	cp $< .
	xxd -i $(<F) > temp4 
	head -n -1 temp4 > $@
	rm $(<F)
	rm temp4

ml_training_data_bdtg_arm1.h:\
	TMVARegression_BDTG.weights.arm1.xml 
	cp $< .
	xxd -i $(<F) > temp5 
	head -n -1 temp5 > $@
	rm $(<F)
	rm temp5

ml_training_data_bdtg_arm1_inner.h:\
	TMVARegression_BDTG.weights.arm1_inner.xml 
	cp $< .
	xxd -i $(<F) > temp6 
	head -n -1 temp6 > $@
	rm $(<F)
	rm temp6

ml_training_data_bdtg_arm1_outer.h:\
	TMVARegression_BDTG.weights.arm1_outer.xml 
	cp $< .
	xxd -i $(<F) > temp7 
	head -n -1 temp7 > $@
	rm $(<F)
	rm temp7

ml_training_data_bdtg_arm1_E33_0.h:\
	TMVARegression_BDTG.weights.arm1_E33_0.xml 
	cp $< .
	xxd -i $(<F) > temp8 
	head -n -1 temp8 > $@
	rm $(<F)
	rm temp8

ml_training_data_mlp_arm0.h:\
	TMVARegression_MLP.weights.arm0.xml 
	cp $< .
	xxd -i $(<F) > temp9 
	head -n -1 temp9 > $@
	rm $(<F)
	rm temp9

ml_training_data_mlp_arm0_inner.h:\
	TMVARegression_MLP.weights.arm0_inner.xml 
	cp $< .
	xxd -i $(<F) > temp10 
	head -n -1 temp10 > $@
	rm $(<F)
	rm temp10

ml_training_data_mlp_arm0_outer.h:\
	TMVARegression_MLP.weights.arm0_outer.xml 
	cp $< .
	xxd -i $(<F) > temp11 
	head -n -1 temp11 > $@
	rm $(<F)
	rm temp11

ml_training_data_mlp_arm0_E33_0.h:\
	TMVARegression_MLP.weights.arm0_E33_0.xml 
	cp $< .
	xxd -i $(<F) > temp12 
	head -n -1 temp12 > $@
	rm $(<F)
	rm temp12

ml_training_data_mlp_arm1.h:\
	TMVARegression_MLP.weights.arm1.xml 
	cp $< .
	xxd -i $(<F) > temp13 
	head -n -1 temp13 > $@
	rm $(<F)
	rm temp13

ml_training_data_mlp_arm1_inner.h:\
	TMVARegression_MLP.weights.arm1_inner.xml 
	cp $< .
	xxd -i $(<F) > temp14 
	head -n -1 temp14 > $@
	rm $(<F)
	rm temp14

ml_training_data_mlp_arm1_outer.h:\
	TMVARegression_MLP.weights.arm1_outer.xml 
	cp $< .
	xxd -i $(<F) > temp15 
	head -n -1 temp15 > $@
	rm $(<F)
	rm temp15

ml_training_data_mlp_arm1_E33_0.h:\
	TMVARegression_MLP.weights.arm1_E33_0.xml 
	cp $< .
	xxd -i $(<F) > temp16 
	head -n -1 temp16 > $@
	rm $(<F)
	rm temp16


clean-local:
	rm -f ${BUILT_SOURCES} *Dict*
