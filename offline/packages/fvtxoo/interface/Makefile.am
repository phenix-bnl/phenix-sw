
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libfvtxoo_interface.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libfvtxoo_interface_la_LIBADD = \
  -lfvtxoo_base \
  -lhalf \
  -lmutoo_interface \
  -lsvx

include_HEADERS = \
  TFvtxClus.h \
  TFvtxClus_v1.h \
  TFvtxClusMap.h \
  TFvtxCoord.h \
  TFvtxCoord_v1.h \
  TFvtxCoordMap.h \
  TFvtxCompactCoord.h \
  TFvtxCompactCoord_v1.h \
  TFvtxCompactCoordMap.h \
  TFvtxEval_v1.h \
  TFvtxEvalMap.h \
  TFvtxHit.h \
  TFvtxHit_v1.h \
  TFvtxHitMap.h \
  TFvtxMCHit.h \
  TFvtxMCHit_v1.h \
  TFvtxMCHitMap.h \
  TFvtxMCStrip.h \
  TFvtxMCStrip_v1.h \
  TFvtxPisaHit.h \
  TFvtxPisaHit_v1.h \
  TFvtxPisaHitMap.h \
  TFvtxQuadraticTrackFit.h \
  TFvtxQuadraticTrkPar.h \
  TFvtxQuadraticTrkPar_v1.h \
  TFvtxQuadraticTrkParMap.h \
  TFvtxResidual.h \
  TFvtxResidual_v1.h \
  TFvtxResidualMap.h \
  TFvtxStraightTrkPar.h \
  TFvtxStraightTrkPar_v1.h \
  TFvtxStraightTrkParMap.h \
  TFvtxSvxCluster.h \
  TFvtxSvxClusterMap.h \
  TFvtxSvxCluster_v4.h \
  TFvtxTrigEmulInfo.h \
  TFvtxTrigEmulInfo_v1.h \
  TFvtxTrk.h \
  TFvtxTrk_v1.h \
  TFvtxTrkMap.h \
  TFvtxCompactTrk.h \
  TFvtxCompactTrk_v1.h \
  TFvtxCompactTrk_v2.h \
  TFvtxCompactTrk_v3.h \
  TFvtxCompactTrk_v4.h \
  TFvtxCompactTrkMap.h \
  TFvtxTrkEval.h 

libfvtxoo_interface_la_SOURCES = \
  TFvtxClus.cxx \
  TFvtxClusDict.C \
  TFvtxClus_v1.cxx \
  TFvtxClus_v1Dict.C \
  TFvtxClusMap.cxx \
  TFvtxCoord.cxx \
  TFvtxCoordDict.C \
  TFvtxCoord_v1.cxx \
  TFvtxCoord_v1Dict.C \
  TFvtxCoordMap.cxx \
  TFvtxCompactCoord.cxx \
  TFvtxCompactCoordDict.C \
  TFvtxCompactCoord_v1.cxx \
  TFvtxCompactCoord_v1Dict.C \
  TFvtxCompactCoordMap.cxx \
  TFvtxEval_v1.cxx \
  TFvtxEval_v1Dict.C \
  TFvtxEvalMap.cxx \
  TFvtxHit.cxx \
  TFvtxHitDict.C \
  TFvtxHit_v1.cxx \
  TFvtxHit_v1Dict.C \
  TFvtxHitMap.cxx \
  TFvtxMCHit.cxx \
  TFvtxMCHitDict.C \
  TFvtxMCHit_v1.cxx \
  TFvtxMCHit_v1Dict.C \
  TFvtxMCHitMap.cxx \
  TFvtxMCStrip.cxx \
  TFvtxMCStripDict.C \
  TFvtxMCStrip_v1.cxx \
  TFvtxMCStrip_v1Dict.C \
  TFvtxQuadraticTrackFit.cxx \
  TFvtxQuadraticTrkPar.C \
  TFvtxQuadraticTrkPar_v1.C \
  TFvtxQuadraticTrkPar_v1Dict.C \
  TFvtxQuadraticTrkParDict.C \
  TFvtxQuadraticTrkParMap.cxx \
  TFvtxPisaHit.cxx \
  TFvtxPisaHitDict.C \
  TFvtxPisaHit_v1.cxx \
  TFvtxPisaHit_v1Dict.C \
  TFvtxPisaHitMap.cxx \
  TFvtxResidual.C \
  TFvtxResidualDict.C \
  TFvtxResidual_v1.C \
  TFvtxResidual_v1Dict.C \
  TFvtxResidualMap.cxx \
  TFvtxStraightTrkPar.C \
  TFvtxStraightTrkParDict.C \
  TFvtxStraightTrkPar_v1.C \
  TFvtxStraightTrkPar_v1Dict.C \
  TFvtxStraightTrkParMap.cxx \
  TFvtxSvxCluster.cxx \
  TFvtxSvxClusterDict.C \
  TFvtxSvxClusterMap.cxx \
  TFvtxSvxCluster_v4.cxx \
  TFvtxSvxCluster_v4Dict.C \
  TFvtxTrigEmulInfo.C \
  TFvtxTrigEmulInfoDict.C \
  TFvtxTrigEmulInfo_v1.C \
  TFvtxTrigEmulInfo_v1Dict.C \
  TFvtxTrk.cxx \
  TFvtxTrkDict.C \
  TFvtxTrk_v1.cxx \
  TFvtxTrk_v1Dict.C \
  TFvtxTrkMap.cxx  \
  TFvtxCompactTrk.cxx \
  TFvtxCompactTrkDict.C \
  TFvtxCompactTrk_v1.cxx \
  TFvtxCompactTrk_v1Dict.C \
  TFvtxCompactTrk_v2.cxx \
  TFvtxCompactTrk_v2Dict.C \
  TFvtxCompactTrk_v3.cxx \
  TFvtxCompactTrk_v3Dict.C \
  TFvtxCompactTrk_v4.cxx \
  TFvtxCompactTrk_v4Dict.C \
  TFvtxCompactTrkMap.cxx  \
  TFvtxTrkEval.cxx \
  TFvtxTrkEvalDict.C 

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = \
  libfvtxoo_interface.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

