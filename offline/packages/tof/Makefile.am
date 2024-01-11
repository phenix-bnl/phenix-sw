AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libtof.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CFLAGS = -Wall -Werror
AM_CXXFLAGS = ${AM_CFLAGS}

libtof_la_LIBADD = \
  -lWrappers \
  -lgea \
  -lgsl \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -luti 

include_HEADERS = \
  Tof.hh \
  TofAddressObject.hh \
  TofBasicObject.hh \
  TofCalibObject.hh \
  TofEvent.hh \
  TofGeometryObject.hh \
  dTofAssocPar.h \
  dTofAssocParWrapper.h \
  dTofAssociate.h \
  dTofAssociateWrapper.h \
  dTofCal.h \
  dTofCalPar.h \
  dTofCalParWrapper.h \
  dTofCalWrapper.h \
  dTofDCM.h \
  dTofDCMPar.h \
  dTofDCMParWrapper.h \
  dTofDCMWrapper.h \
  dTofEval.h \
  dTofEvalPar.h \
  dTofEvalParWrapper.h \
  dTofEvalWrapper.h \
  dTofFEM.h \
  dTofFEMWrapper.h \
  dTofFEMhitGhit.h \
  dTofFEMhitGhitWrapper.h \
  dTofFEMmap.h \
  dTofFEMmapWrapper.h \
  dTofGdigi.h \
  dTofGdigiRec.h \
  dTofGdigiRecWrapper.h \
  dTofGdigiWrapper.h \
  dTofGeo.h \
  dTofGeoPar.h \
  dTofGeoParWrapper.h \
  dTofGeoWrapper.h \
  dTofGhitGdigi.h \
  dTofGhitGdigiWrapper.h \
  dTofGhitRaw.h \
  dTofGhitRawPar.h \
  dTofGhitRawParWrapper.h \
  dTofGhitRawWrapper.h \
  dTofPerfPar.h \
  dTofPerfParWrapper.h \
  dTofPid.h \
  dTofPidPar.h \
  dTofPidParWrapper.h \
  dTofPidWrapper.h \
  dTofRaw.h \
  dTofRawRec.h \
  dTofRawRecPar.h \
  dTofRawRecParWrapper.h \
  dTofRawRecWrapper.h \
  dTofRawWrapper.h \
  dTofReconstructed.h \
  dTofReconstructedWrapper.h\
  dTofUcal.h \
  dTofUcalPar.h \
  dTofUcalParWrapper.h \
  dTofUcalWrapper.h \
  mTofGhitRawModule.h \
  mTofDCMModule.h \
  mTofEvaluateModule.h \
  mTofFEMModule.h \
  mTofGhitGdigiModule.h \
  TofOut.h \
  TofOutv1.h \
  TofOutv2.h \
  TofPutDCM.h \
  TofHit.h
noinst_HEADERS = \
  mTofDCM.h \
  mTofFEM.h \
  mTofGdigiRecModule.h \
  mTofPerfectModule.h \
  mTofSetCalModule.h \
  mTofSetFEMmap.h \
  mTofSetFEMmapModule.h \
  mTofSetGeoModule.h \
  mTofSetGeoParModule.h \
  mTofSetUcalModule.h \
  tof_FEM.h \
  TofHitv1.h \
  TofHitv2.h \
  libtof_LinkDef.h

libtof_la_SOURCES = \
  TofAddressObject.cc \
  TofBasicObject.cc \
  TofCalibObject.cc \
  TofEvent.cc \
  TofGeometryObject.cc \
  TofGetDCM.cc \
  dTofAssocParWrapper.C \
  dTofAssociateWrapper.C \
  dTofCalParWrapper.C \
  dTofCalWrapper.C \
  dTofDCMParWrapper.C \
  dTofDCMWrapper.C \
  dTofEvalParWrapper.C \
  dTofEvalWrapper.C \
  dTofFEMWrapper.C \
  dTofFEMhitGhitWrapper.C \
  dTofFEMmapWrapper.C \
  dTofGdigiRecWrapper.C \
  dTofGdigiWrapper.C \
  dTofGeoParWrapper.C \
  dTofGeoWrapper.C \
  dTofGhitGdigiWrapper.C \
  dTofGhitRawParWrapper.C \
  dTofGhitRawWrapper.C \
  dTofPerfParWrapper.C \
  dTofPidParWrapper.C \
  dTofPidWrapper.C \
  dTofRawRecParWrapper.C \
  dTofRawRecWrapper.C \
  dTofRawWrapper.C \
  dTofReconstructedWrapper.C \
  dTofUcalParWrapper.C \
  dTofUcalWrapper.C \
  mTofDCM.C \
  mTofDCMModule.C \
  mTofDCMPAM.C \
  mTofEvaluateModule.C \
  mTofFEM.c \
  mTofFEMModule.C \
  mTofFEMPAM.C \
  mTofGdigiRecModule.C \
  mTofGhitGdigiModule.C \
  mTofGhitRawModule.C \
  mTofPerfectModule.C \
  mTofSetCalModule.C \
  mTofSetFEMmap.c \
  mTofSetFEMmapModule.C \
  mTofSetFEMmapPAM.C\
  mTofSetGeoModule.C \
  mTofSetGeoParModule.C \
  mTofSetUcalModule.C \
  TofOut.C \
  TofOutv1.C \
  TofOutv2.C \
  TofPutDCM.cc \
  TofHit.C \
  TofHitv1.C \
  TofHitv2.C
nodist_libtof_la_SOURCES = libtof_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libtof.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libtof_Dict.C:\
  dTofAssocParWrapper.h \
  dTofAssociateWrapper.h \
  dTofCalWrapper.h \
  dTofCalParWrapper.h \
  dTofDCMWrapper.h \
  dTofDCMParWrapper.h \
  dTofEvalWrapper.h \
  dTofEvalParWrapper.h \
  dTofFEMWrapper.h \
  dTofFEMhitGhitWrapper.h \
  dTofFEMmapWrapper.h \
  dTofGdigiWrapper.h \
  dTofGdigiRecWrapper.h \
  dTofGeoWrapper.h \
  dTofGeoParWrapper.h \
  dTofGhitGdigiWrapper.h \
  dTofGhitRawWrapper.h \
  dTofGhitRawParWrapper.h \
  dTofPerfParWrapper.h \
  dTofPidWrapper.h \
  dTofPidParWrapper.h \
  dTofRawWrapper.h \
  dTofRawRecWrapper.h \
  dTofRawRecParWrapper.h \
  dTofReconstructedWrapper.h \
  dTofUcalWrapper.h \
  dTofUcalParWrapper.h \
  mTofDCMModule.h \
  mTofEvaluateModule.h \
  mTofFEMModule.h \
  mTofGdigiRecModule.h \
  mTofGhitGdigiModule.h \
  mTofGhitRawModule.h \
  mTofPerfectModule.h \
  mTofSetCalModule.h \
  mTofSetFEMmapModule.h \
  mTofSetGeoModule.h \
  mTofSetGeoParModule.h \
  mTofSetUcalModule.h \
  TofOut.h \
  TofOutv1.h \
  TofOutv2.h \
  TofHit.h \
  TofHitv1.h \
  TofHitv2.h \
  TofAddressObject.hh \
  TofBasicObject.hh \
  TofCalibObject.hh \
  TofGeometryObject.hh \
  libtof_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict*
