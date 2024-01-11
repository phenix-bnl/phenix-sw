AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libpad.la 

AM_CFLAGS = -Wall -Werror
AM_CXXFLAGS = $(AM_CFLAGS)
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libpad_la_LIBADD = \
  -lgea \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -luti \
  -lWrappers

noinst_HEADERS = \
  PadSnglClusterv1.h \
  PadSnglClusterv2.h \
  PadSnglRawv1.h \
  mPadFastSim.h \
  mPadFEM.h \
  mPadDCM.h \
  mPadRec.h \
  mPadUnpack.h \
  mPadDumpRaw.h \
  PadGetDCM.h \
  PadAncObjy.hh \
  padMixDST.hh \
  PadRecCell.hh \
  mPadEvaluate.h \
  libpad_LinkDef.h

include_HEADERS = \
  PadAddressObject.hh \
  PadBasicObject.hh \
  PadCalibrationObject.hh \
  PadFEM.h \
  PadRec.hh \
  PadRecModule.hh \
  PadOnlineRec.h \
  PadVertexFunction.h \
  dPad23Par.h \
  dPad23ParWrapper.h \
  dPadCluster.h \
  dPadClusterWrapper.h \
  dPadDCM.h \
  dPadDCMPar.h \
  dPadDCMParWrapper.h \
  dPadDCMWrapper.h \
  dPadEval.h \
  dPadEvalPar.h \
  dPadEvalParWrapper.h \
  dPadEvalWrapper.h \
  dPadFEM.h \
  dPadFEMPar.h \
  dPadFEMParWrapper.h \
  dPadFEMWrapper.h \
  dPadFastSimPar.h \
  dPadFastSimParWrapper.h \
  dPadGeom.h \
  dPadGeomWrapper.h \
  dPadGhitClus.h \
  dPadGhitClusWrapper.h \
  dPadGhitRaw.h \
  dPadGhitRawWrapper.h \
  dPadNibbleGhit.h \
  dPadNibbleGhitWrapper.h \
  dPadPerfPar.h \
  dPadPerfParWrapper.h \
  dPadRaw.h \
  dPadRawClus.h \
  dPadRawClusWrapper.h \
  dPadRawWrapper.h \
  dPadRecPar.h \
  dPadRecParWrapper.h \
  dPadSlowSimPar.h \
  dPadSlowSimParWrapper.h \
  mPadDCMModule.h \
  mPadDumpRawModule.h \
  mPadEvaluateModule.h \
  mPadFEMModule.h \
  mPadFastSimModule.h \
  mPadRecModule.h \
  mPadSlowSimModule.h \
  mPadUnpackModule.h \
  padDetectorGeo.hh \
  padEvtToRaw.hh \
  padInclBad.hh \
  PadSnglCluster.h \
  PadCluster.h \
  PadClusterv1.h \
  PadClusterv2.h \
  PadPutDCM.h \
  PadTableSet.h \
  PadRaw.h \
  PadRawv1.h \
  PadSnglRaw.h

libpad_la_SOURCES = \
  PadAddressObject.cc \
  PadAncObjy.cc \
  PadBasicObject.cc \
  PadCalibrationObject.cc \
  PadFEM.c \
  PadGetDCM.cc \
  PadPutDCM.cc \
  PadRec.cc \
  PadRecModule.cc \
  PadTableSet.cc \
  PadOnlineRec.cc \
  PadVertexFunction.cc \
  dPad23ParWrapper.C \
  dPadClusterWrapper.C \
  dPadDCMParWrapper.C \
  dPadDCMWrapper.C \
  dPadEvalParWrapper.C \
  dPadEvalWrapper.C \
  dPadFEMParWrapper.C \
  dPadFEMWrapper.C \
  dPadFastSimParWrapper.C \
  dPadGeomWrapper.C \
  dPadGhitClusWrapper.C \
  dPadGhitRawWrapper.C \
  dPadNibbleGhitWrapper.C \
  dPadPerfParWrapper.C \
  dPadRawClusWrapper.C \
  dPadRawWrapper.C \
  dPadRecParWrapper.C \
  dPadSlowSimParWrapper.C \
  mPadDCM.c \
  mPadDCMModule.C \
  mPadDCMPAM.C \
  mPadDumpRaw.c \
  mPadDumpRawModule.C \
  mPadDumpRawPAM.C \
  mPadEvaluate.cc \
  mPadEvaluateModule.C \
  mPadEvaluatePAM.C \
  mPadFEM.c \
  mPadFEMModule.C \
  mPadFEMPAM.C \
  mPadFastSim.c \
  mPadFastSimModule.C \
  mPadFastSimPAM.C \
  mPadRec.c \
  mPadRecModule.C \
  mPadRecPAM.C \
  mPadSlowSimModule.C \
  mPadUnpack.c \
  mPadUnpackModule.C \
  mPadUnpackPAM.C \
  padDetectorGeo.cc \
  padEvtToRaw.cc \
  padInclBad.cc \
  padMixDST.cc \
  PadCluster.C \
  PadClusterv1.C \
  PadSnglClusterv1.C \
  PadClusterv2.C \
  PadSnglCluster.C \
  PadSnglClusterv2.C \
  PadRaw.C \
  PadRawv1.C \
  PadSnglRaw.C \
  PadSnglRawv1.C

nodist_libpad_la_SOURCES = libpad_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libpad.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libpad_Dict.C: \
  dPad23ParWrapper.h \
  dPadClusterWrapper.h \
  dPadDCMParWrapper.h \
  dPadDCMWrapper.h \
  dPadEvalParWrapper.h \
  dPadEvalWrapper.h \
  dPadFastSimParWrapper.h \
  dPadFEMParWrapper.h \
  dPadFEMWrapper.h \
  dPadGeomWrapper.h \
  dPadGhitClusWrapper.h \
  dPadGhitRawWrapper.h \
  dPadNibbleGhitWrapper.h \
  dPadPerfParWrapper.h \
  dPadRawClusWrapper.h \
  dPadRawWrapper.h \
  dPadRecParWrapper.h \
  dPadSlowSimParWrapper.h \
  mPadDCMModule.h \
  mPadDumpRawModule.h \
  mPadEvaluateModule.h \
  mPadFastSimModule.h \
  mPadFEMModule.h \
  mPadRecModule.h \
  mPadUnpackModule.h \
  PadAddressObject.hh \
  PadAncObjy.hh \
  PadBasicObject.hh \
  PadCalibrationObject.hh \
  PadCluster.h \
  PadClusterv1.h \
  PadClusterv2.h \
  padEvtToRaw.hh \
  padInclBad.hh \
  padMixDST.hh \
  PadOnlineRec.h \
  PadRaw.h \
  PadRawv1.h \
  PadRecModule.hh \
  PadSnglCluster.h \
  PadSnglClusterv1.h \
  PadSnglClusterv2.h \
  PadSnglRaw.h \
  PadSnglRawv1.h \
  PadVertexFunction.h \
  libpad_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
