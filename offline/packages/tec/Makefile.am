AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libtec.la

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libtec_la_LIBADD = \
  -lphool \
  -lphflag \
  -lphgeo \
  -ltof

noinst_HEADERS = \
  half.h \
  TecGetDCM.h \
  libtec_LinkDef.h

include_HEADERS = \
  GetUniqueName.h \
  TDragZoomTH2F.hh \
  TSignalingTButton.hh \
  TTextInputDialog.hh \
  TecAddressObject.hh \
  TecBasicObject.hh \
  TecCalibrationObject.hh \
  TecHVObject.hh \
  TecPacket.hh \
  TecPacketList.hh \
  TecPutDCM.h \
  TecGHit.hh \
  TecGHitList.hh \
  TecGeometryObject.hh \
  TecHit.hh \
  TecCluster.hh \
  TecClusterV1.hh \
  TecClusterContainer.hh \
  TecClusterContainerV1.hh \
  TecSnglProj.hh \
  TecSnglProjv1.hh \
  TecProj.hh \
  TecProjv1.hh \
  TecOut.hh \
  TecOutV1.hh \
  TecOutV2.hh \
  TecOutV3.hh \
  TecOutV4.hh \
  TecShortHit.hh \
  TecTrack.hh \
  TecTrackTR.hh \
  TecTrackTRv1.hh \
  TecTrackTRv2.hh \
  TecWire.hh \
  TecWires.hh \
  dTecDcmData.h \
  dTecDcmDataWrapper.h \
  dTecFemData.h \
  dTecFemDataWrapper.h \
  dTecGhitDcm.h \
  dTecGhitDcmWrapper.h \
  dTecGhitHits.h \
  dTecGhitHitsWrapper.h \
  dTecGhitRaw.h \
  dTecGhitRawWrapper.h \
  dTecRaw.h \
  dTecRawWrapper.h \
  dTecRawTrack.h \
  dTecRawTrackWrapper.h \
  dTecTrack.h \
  dTecTrackWrapper.h \
  mTecUtilities.h \
  mTecAnalysisModule.h \
  mTecCalibModule.h \
  mTecUnpackModule.h \
  mTecDecodeModule.h \
  mTecDrawModule.h \
  mTecGainsModule.h \
  mTecHoughTrackModule.h \
  mTecPackModule.h \
  mTecSlowSimModule.h \
  mTecHitClusteringModule.h \
  TRDLike.hh

libtec_la_SOURCES = \
  GetUniqueName.cc \
  half.C \
  TDragZoomTH2F.cc \
  TSignalingTButton.cc \
  TTextInputDialog.cc \
  TecAddressObject.cc \
  TecBasicObject.cc \
  TecCalibrationObject.cc \
  TecHVObject.cc \
  TecGetDCM.cc \
  TecPacket.cc \
  TecPacketList.cc \
  TecPutDCM.cc \
  TecGHit.cc \
  TecGHitList.cc \
  TecGeometryObject.cc \
  TecHit.cc \
  TecCluster.cc \
  TecClusterV1.cc \
  TecClusterContainer.cc \
  TecClusterContainerV1.cc \
  TecSnglProj.cc \
  TecSnglProjv1.cc \
  TecProj.cc \
  TecProjv1.cc \
  TecOut.C \
  TecOutV1.cc \
  TecOutV2.cc \
  TecOutV3.cc \
  TecOutV4.cc \
  TecShortHit.cc \
  TecTrack.cc \
  TecTrackTR.cc \
  TecTrackTRv1.cc \
  TecTrackTRv2.cc \
  TecWire.cc \
  TecWires.cc \
  dTecDcmDataWrapper.C \
  dTecFemDataWrapper.C \
  dTecGhitDcmWrapper.C \
  dTecGhitHitsWrapper.C \
  dTecGhitRawWrapper.C \
  dTecRawWrapper.C \
  dTecRawTrackWrapper.C \
  dTecTrackWrapper.C \
  mTecUtilities.cc \
  mTecAnalysisModule.cc \
  mTecCalibModule.cc \
  mTecUnpackModule.cc \
  mTecDecodeModule.cc \
  mTecDrawModule.cc \
  mTecGainsModule.cc \
  mTecHoughTrackModule.cc \
  mTecPackModule.cc \
  mTecSlowSimModule.cc \
  mTecHitClusteringModule.cc \
  TRDLike.C

nodist_libtec_la_SOURCES = libtec_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libtec.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libtec_Dict.C: \
  dTecDcmDataWrapper.h \
  dTecFemDataWrapper.h \
  dTecGhitDcmWrapper.h \
  dTecGhitHitsWrapper.h \
  dTecGhitRawWrapper.h \
  dTecRawTrackWrapper.h \
  dTecRawWrapper.h \
  dTecTrackWrapper.h \
  TDragZoomTH2F.hh \
  TecAddressObject.hh \
  TecBasicObject.hh \
  TecCalibrationObject.hh \
  TecClusterContainer.hh \
  TecClusterContainerV1.hh \
  TecCluster.hh \
  TecClusterV1.hh \
  TecGeometryObject.hh \
  TecGHit.hh \
  TecGHitList.hh \
  TecHit.hh \
  TecHVObject.hh \
  TecOut.hh \
  TecOutV1.hh \
  TecOutV2.hh \
  TecOutV3.hh \
  TecOutV4.hh \
  TecPacket.hh \
  TecPacketList.hh \
  TecProj.hh \
  TecProjv1.hh \
  TecShortHit.hh \
  TecSnglProj.hh \
  TecSnglProjv1.hh \
  TecTrack.hh \
  TecTrackTR.hh \
  TecTrackTRv1.hh \
  TecTrackTRv2.hh \
  TecWire.hh \
  TecWires.hh \
  TRDLike.hh \
  TSignalingTButton.hh \
  TTextInputDialog.hh \
  libtec_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict*
