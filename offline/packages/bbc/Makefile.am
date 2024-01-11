AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libbbc.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libbbc_la_LIBADD = \
  -lRootmessage \
  -lWrappers \
  -lgea \
  -lgsl \
  -lpdbcalBase \
  -lphool \
  -lphflag \
  -lphtime

noinst_HEADERS = \
  mBbcDCM.h \
  mBbcFEM.h \
  mBbcGhitRaw.h \
  mBbcOutEvalModule.h \
  mBbcSetGeo.h \
  mBbcUnpackMDC2Module.h \
  libbbc_LinkDef.h

include_HEADERS = \
  mBbcUnpackModule.h \
  mBbcSetGeoModule.h \
  mBbcRawOutModule.h \
  mBbcDCMModule.h \
  mBbcFEMModule.h \
  mBbcGhitRawModule.h \
  mBbcSetUcalModule.h \
  Bbc.hh \
  BbcCalib.hh \
  BbcCalibPar.hh \
  BbcGetDCM.h \
  BbcEvent.hh \
  BbcEventStorage.hh \
  BbcGeo.hh \
  BbcPutDCM.h \
  BbcPutDCMReCal.h \
  CalibratorFitPar.hh \
  CalibratorPeak.hh \
  bbc_common.h \
  dBbcCal.h \
  dBbcCalWrapper.h \
  dBbcDCM.h \
  dBbcDCMMDC2.h \
  dBbcDCMMDC2Wrapper.h \
  dBbcDCMWrapper.h \
  dBbcEval.h \
  dBbcEvalWrapper.h \
  dBbcFEM.h \
  dBbcFEMWrapper.h \
  dBbcGeo.h \
  dBbcGeoWrapper.h \
  dBbcGhitRaw.h \
  dBbcGhitRawPar.h \
  dBbcGhitRawParWrapper.h \
  dBbcGhitRawWrapper.h \
  dBbcOut.h \
  dBbcOutWrapper.h \
  dBbcRaw.h \
  dBbcRawHitPar.h \
  dBbcRawHitParWrapper.h \
  dBbcRawWrapper.h \
  dBbcUcal.h \
  dBbcUcalWrapper.h \
  BbcReturncodes.h \
  BbcRaw.h \
  BbcRawv1.h \
  BbcRawHit.h \
  BbcOut.h \
  BbcOutv1.h \
  mBbcRawReCal.h \
  BbcNorthSouth.h

libbbc_la_SOURCES = \
  BbcCalib.cc \
  BbcCalibPar.cc \
  BbcEvent.cc \
  BbcEventStorage.cc \
  BbcGeo.cc \
  BbcGetDCM.cc \
  BbcPutDCM.cc \
  BbcPutDCMReCal.cc \
  CalibratorFitPar.cc \
  CalibratorPeak.cc \
  dBbcCalWrapper.C \
  dBbcDCMMDC2Wrapper.C \
  dBbcDCMWrapper.C \
  dBbcEvalWrapper.C \
  dBbcFEMWrapper.C \
  dBbcGeoWrapper.C \
  dBbcGhitRawParWrapper.C \
  dBbcGhitRawWrapper.C \
  dBbcOutWrapper.C \
  dBbcRawHitParWrapper.C \
  dBbcRawWrapper.C \
  dBbcUcalWrapper.C \
  mBbcDCM.cc \
  mBbcDCMModule.C \
  mBbcDCMPAM.C \
  mBbcFEM.c \
  mBbcFEMModule.C \
  mBbcFEMPAM.C \
  mBbcGhitRaw.cc \
  mBbcGhitRawModule.C \
  mBbcGhitRawPAM.C \
  mBbcOutEvalModule.C \
  mBbcRawOutModule.C \
  mBbcSetGeo.cc \
  mBbcSetGeoModule.C \
  mBbcSetGeoPAM.C \
  mBbcSetUcalModule.C \
  mBbcUnpackMDC2Module.C \
  mBbcUnpackModule.C \
  BbcRaw.C \
  BbcRawv1.C \
  BbcRawHit.C \
  BbcOut.C \
  BbcOutv1.C \
  mBbcRawReCal.C \
  BbcNorthSouth.C

nodist_libbbc_la_SOURCES = libbbc_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libbbc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libbbc_Dict.C: \
  BbcCalib.hh \
  BbcCalibPar.hh \
  BbcEvent.hh \
  BbcEventStorage.hh \
  BbcGeo.hh \
  Bbc.hh \
  BbcOut.h \
  BbcOutv1.h \
  BbcNorthSouth.h \
  BbcRaw.h \
  BbcRawv1.h \
  BbcRawHit.h \
  CalibratorFitPar.hh \
  CalibratorPeak.hh \
  dBbcCalWrapper.h \
  dBbcDCMMDC2Wrapper.h \
  dBbcDCMWrapper.h \
  dBbcEvalWrapper.h \
  dBbcFEMWrapper.h \
  dBbcGeoWrapper.h \
  dBbcGhitRawParWrapper.h \
  dBbcGhitRawWrapper.h \
  dBbcOutWrapper.h \
  dBbcRawHitParWrapper.h \
  dBbcRawWrapper.h \
  dBbcUcalWrapper.h \
  mBbcDCMModule.h \
  mBbcFEMModule.h \
  mBbcGhitRawModule.h \
  mBbcOutEvalModule.h \
  mBbcRawOutModule.h \
  mBbcSetGeoModule.h \
  mBbcSetUcalModule.h \
  mBbcUnpackModule.h \
  mBbcUnpackMDC2Module.h \
  libbbc_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^
clean-local:
	rm -f *Dict*


testexternals_SOURCES = testexternals.C
