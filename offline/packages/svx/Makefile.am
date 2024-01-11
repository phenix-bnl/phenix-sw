AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

# no point in suppressing warnings people should
# at least see them, so here we go for g++: -Wall
AM_CXXFLAGS = -Wall -Werror -msse2

RINCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_CPPFLAGS = -isystem $(includedir) -isystem $(OFFLINE_MAIN)/include -isystem `root-config --incdir` -isystem $(OFFLINE_MAIN)/include/eigen3

lib_LTLIBRARIES = libsvx.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L`root-config --libdir`

libsvx_la_LIBADD = \
  -lnanoDST \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -lPISARoot \
  -lSubsysReco \
  -lfun4all \
  -lgea \
  -lMinuit \
  -lheader \
  -lbbc \
  -lvtx \
  -lEG

noinst_HEADERS = \
  SvxLinkDef.h \
  SvxPisaHitLinkDef.h \
  SvxPisaHitv1LinkDef.h \
  SvxSnglPisaHitLinkDef.h \
  SvxSnglPisaHitv1LinkDef.h \
  SvxTrackLinkDef.h \
  SvxTrackListLinkDef.h \
  svxAddressLinkDef.h \
  svxDetectorGeoLinkDef.h \
  SvxSimulator.h \
  SvxEncode.h \
  PHsvxDetectorGeo.hh \
  SvxGetGEA.h \
  SvxCommon.h \
  SvxTracker.h \
  SvxStandAloneReco.h \
  SvxStandAloneRecoLowMult.h \
  SvxFindHotDead.h \
  SvxCglReco.h \
  SvxPixStructure.h \
  SvxPixStruct.h \
  SvxRecoSingleTrack.h \
  SvxRecoTracks.h \
  SvxSensor.h \
  SvxStripixel.h \
  SvxPacket.h \
  SvxPacketList.h \
  Svx_stripixelPacket.h \
  Svx_stripixelPacketList.h \
  SvxHit.h \
  SvxPisaHit.h \
  SvxPisaHitv1.h \
  SvxSnglPisaHit.h \
  SvxSnglPisaHitv1.h \
  SvxStrip11v1.h \
  SvxStrip11v2.h \
  SvxMap.h \
  SvxMasterReco.h \
  SvxPriVertexSeedFinder.h \
  SvxEventKiller.h \
  SvxVertexCombiner.h \
  SvxERecal.h \
  SvxLinkDef.h

include_HEADERS = \
  PHsvxDetectorGeo.hh \
  SvxClusterContainer.h \
  SvxCluster.h \
  SvxClusterv1.h \
  SvxClusterv2.h \
  SvxClusterv3.h \
  SvxClusterv4.h \
  SvxClusterv5.h \
  SvxClusterList.h \
  SvxClusterListv1.h \
  SvxClusterListv2.h \
  SvxClusterListv3.h \
  SvxClusterListv4.h \
  SvxClusterListv5.h \
  SvxGetGEA.h \
  SvxGhit.h \
  SvxGhitv1.h \
  SvxGhitList.h \
  SvxGhitListv1.h \
  SvxGhitCluster.h \
  SvxGhitClusterv1.h \
  SvxGhitClusterList.h \
  SvxGhitClusterListv1.h \
  SvxGhitRawhit.h \
  SvxGhitRawhitv1.h \
  SvxGhitRawhitList.h \
  SvxGhitRawhitListv1.h \
  SvxHit.h \
  SvxPisaHit.h \
  SvxPisaHitv1.h \
  SvxRawhit.h \
  SvxRawhitv1.h \
  SvxRawhitv2.h \
  SvxRawhitv3.h \
  SvxRawhitv4.h \
  SvxRawhitv5.h \
  SvxRawhitList.h \
  SvxRawhitListv1.h \
  SvxRawhitListv2.h \
  SvxRawhitListv3.h \
  SvxRawhitListv4.h \
  SvxRawhitListv5.h \
  SvxPixelRawHitList.h \
  SvxStripRawHitList.h \
  SvxConvertPixelRawHitList.h \
  SvxConvertStripRawHitList.h \
  SvxRawhitCluster.h \
  SvxRawhitClusterv1.h \
  SvxRawhitClusterList.h \
  SvxRawhitClusterListv1.h \
  SvxRecoSingleTrack.h \
  SvxRecoTracks.h \
  SvxStripThreshold.h \
  SvxTrackProj.h \
  SvxTrackProj_v1.h \
  SvxTrackFit.h \
  SvxTrackFit_v1.h \
  SvxTrack.h \
  SvxTrack_v1.h \
  SvxTrackList.h \
  SvxTrackList_v1.h \
  SvxSegment.h \
  SvxSegmentv1.h \
  SvxSegmentv2.h \
  SvxSegmentv3.h \
  SvxSegmentv4.h \
  SvxSegmentv5.h \
  SvxSegmentv6.h \
  SvxSegmentList.h \
  SvxSegmentListv1.h \
  SvxSegmentListv2.h \
  SvxSegmentListv3.h \
  SvxSegmentListv4.h \
  SvxSegmentListv5.h \
  SvxSegmentListv6.h \
  SvxEventInfo.h \
  SvxEventInfov1.h \
  SvxEventInfov2.h \
  SvxEventInfov3.h \
  SvxEventInfov4.h \
  SvxClusterInfo.h \
  SvxClusterInfov1.h \
  SvxClusterInfov2.h \
  SvxClusterInfov3.h \
  SvxClusterInfov4.h \
  SvxClusterInfov5.h \
  SvxResidualInfo.h \
  SvxResidualInfov1.h \
  SvxCentralTrack.h \
  SvxCentralTrackv1.h \
  SvxCentralTrackv2.h \
  SvxCentralTrackv3.h \
  SvxCentralTrackv4.h \
  SvxCentralTrackv5.h \
  SvxCentralTrackv6.h \
  SvxCentralTrackv7.h \
  SvxCentralTrackv8.h \
  SvxCentralTrackv9.h \
  SvxCentralTrackList.h \
  SvxCentralTrackListv1.h \
  SvxCentralTrackListv2.h \
  SvxCentralTrackListv3.h \
  SvxCentralTrackListv4.h \
  SvxCentralTrackListv5.h \
  SvxCentralTrackListv6.h \
  SvxCentralTrackListv7.h \
  SvxCentralTrackListv8.h \
  SvxCentralTrackListv9.h \
  SvxSensor.h \
  SvxSnglPisaHit.h \
  SvxSnglPisaHitv1.h \
  SvxPixel1v1.h \
  SvxTracker.h \
  SvxParameters.h \
  svxDetectorGeo.hh \
  SvxParManager.h  \
  SvxDeadMap.h \
  SvxApplyHotDead.h  \
  SvxMergeRawHits.h  \
  SvxSelectClusters.h  \
  svxAddress.hh  \
  SvxOrganizedHits.h \
  SvxVertexReco.h \
  SvxDecode.h  \
  SvxReco.h  \
  SvxStandAloneReco.h \
  SvxStandAloneRecoLowMult.h \
  SvxMasterReco.h \
  SvxCentralTrackReco.h \
  SvxPixelHotDeadMap.h \
  SvxPixelHotDeadMapv2.h \
  SvxBeamCenterPar.h  \
  SvxDaqErrorMap.h \
  SvxPriVertexSeedFinder.h \
  SvxStripFindHotDead.h \
  SvxPrimVertexFinder.h \
  SvxPrimVertexFinderLowMult.h \
  SvxPrimVertexInfo.h \
  SvxPrimVertexInfov1.h \
  SvxComponentGeom.h 
DICTS := \
  SvxPisaHitDict.C \
  SvxPisaHitv1Dict.C \
  SvxSnglPisaHitDict.C \
  SvxSnglPisaHitv1Dict.C \
  PHsvxDetectorGeoDict.C \
  SvxTrack_Dict.C \
  SvxTrackList_Dict.C \
  svxDetectorGeoDict.C \
  svxAddressDict.C \
  Svx_Dict.C 

nodist_libsvx_la_SOURCES = $(DICTS)
BUILT_SOURCES += $(DICTS)

libsvx_la_SOURCES = \
  PHsvxDetectorGeo.cc \
  SvxCommon.C \
  SvxHit.C \
  SvxGhit.C \
  SvxGhitv1.C \
  SvxGhitList.C \
  SvxGhitListv1.C \
  SvxPisaHit.C \
  SvxPisaHitv1.C \
  SvxSnglPisaHit.C \
  SvxSnglPisaHitv1.C \
  SvxGetGEA.cc \
  SvxPacket.C \
  SvxPacketList.C \
  Svx_stripixelPacket.C \
  Svx_stripixelPacketList.C \
  SvxRawhit.C \
  SvxRawhitv1.C \
  SvxRawhitv2.C \
  SvxRawhitv3.C \
  SvxRawhitv4.C \
  SvxRawhitv5.C \
  SvxRawhitList.C \
  SvxRawhitListv1.C \
  SvxRawhitListv2.C \
  SvxRawhitListv3.C \
  SvxRawhitListv4.C \
  SvxRawhitListv5.C \
  SvxPixelRawHitList.C \
  SvxStripRawHitList.C \
  SvxConvertPixelRawHitList.C \
  SvxConvertStripRawHitList.C \
  SvxGhitRawhit.C \
  SvxGhitRawhitv1.C \
  SvxGhitRawhitList.C \
  SvxGhitRawhitListv1.C \
  SvxCluster.C \
  SvxClusterv1.C \
  SvxClusterv2.C \
  SvxClusterv3.C \
  SvxClusterv4.C \
  SvxClusterv5.C \
  SvxClusterList.C \
  SvxClusterListv1.C \
  SvxClusterListv2.C \
  SvxClusterListv3.C \
  SvxClusterListv4.C \
  SvxClusterListv5.C \
  SvxRawhitCluster.C \
  SvxRawhitClusterv1.C \
  SvxRawhitClusterList.C \
  SvxRawhitClusterListv1.C \
  SvxGhitCluster.C \
  SvxGhitClusterv1.C \
  SvxGhitClusterList.C \
  SvxGhitClusterListv1.C \
  SvxTrackProj.C \
  SvxTrackProj_v1.C \
  SvxTrackFit.C \
  SvxTrackFit_v1.C \
  SvxTrack.C \
  SvxTrack_v1.C \
  SvxTrackList.C \
  SvxTrackList_v1.C \
  SvxSegment.C \
  SvxSegmentv1.C \
  SvxSegmentv2.C \
  SvxSegmentv3.C \
  SvxSegmentv4.C \
  SvxSegmentv5.C \
  SvxSegmentv6.C \
  SvxSegmentList.C \
  SvxSegmentListv1.C \
  SvxSegmentListv2.C \
  SvxSegmentListv3.C \
  SvxSegmentListv4.C \
  SvxSegmentListv5.C \
  SvxSegmentListv6.C \
  SvxEventInfov1.C \
  SvxEventInfov2.C \
  SvxEventInfov3.C \
  SvxEventInfov4.C \
  SvxClusterInfo.C \
  SvxClusterInfov1.C \
  SvxClusterInfov2.C \
  SvxClusterInfov3.C \
  SvxClusterInfov4.C \
  SvxClusterInfov5.C \
  SvxResidualInfo.C \
  SvxResidualInfov1.C \
  SvxCentralTrack.C \
  SvxCentralTrackv1.C \
  SvxCentralTrackv2.C \
  SvxCentralTrackv3.C \
  SvxCentralTrackv4.C \
  SvxCentralTrackv5.C \
  SvxCentralTrackv6.C \
  SvxCentralTrackv7.C \
  SvxCentralTrackv8.C \
  SvxCentralTrackv9.C \
  SvxCentralTrackList.C \
  SvxCentralTrackListv1.C \
  SvxCentralTrackListv2.C \
  SvxCentralTrackListv3.C \
  SvxCentralTrackListv4.C \
  SvxCentralTrackListv5.C \
  SvxCentralTrackListv6.C \
  SvxCentralTrackListv7.C \
  SvxCentralTrackListv8.C \
  SvxCentralTrackListv9.C \
  SvxReco.C \
  SvxSimulator.C \
  SvxEncode.C \
  SvxDecode.C \
  SvxPixStructure.C \
  SvxPixStruct.C \
  SvxSensor.C \
  SvxStripixel.C \
  SvxPixel1v1.C \
  SvxStrip11v1.C \
  SvxStrip11v2.C \
  svxDetectorGeo.cc \
  SvxParManager.C \
  SvxApplyHotDead.C \
  SvxMergeRawHits.C \
  SvxSelectClusters.C \
  svxAddress.cc \
  SvxTracker.C \
  SvxVertexReco.C \
  SvxStandAloneReco.C \
  SvxStandAloneRecoLowMult.C \
  SvxDeadMap.C \
  SvxStripThreshold.C \
  SvxPixelHotDeadMap.C \
  SvxPixelHotDeadMapv2.C \
  SvxBeamCenterPar.C \
  SvxDaqErrorMap.C \
  SvxFindHotDead.C \
  SvxCglReco.C \
  SvxMap.C \
  SvxClusterContainer.cc \
  SvxMasterReco.C \
  SvxCentralTrackReco.C \
  SvxPriVertexSeedFinder.C \
  SvxPrimVertexFinder.C \
  SvxPrimVertexFinderLowMult.C \
  SvxPrimVertexInfo.C \
  SvxPrimVertexInfov1.C \
  SvxStripFindHotDead.C \
  SvxEventKiller.C \
  SvxVertexCombiner.C \
  SvxERecal.C \
  SvxComponentGeom.C


noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libsvx.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating table CINT dictionaries.
%Dict.C %Dict.h: %Wrapper.h %LinkDef.h
	rootcint -f $*Dict.C -c $(DEFS) $(DEFAULT_INCLUDES) $(RINCLUDES) $^

%Dict.C %Dict.h: %.h %LinkDef.h
	rootcint -f $*Dict.C -c $(DEFS) $(DEFAULT_INCLUDES) $(RINCLUDES) $^


SvxTrackList_Dict.C: SvxTrackList.h SvxTrackList_v1.h SvxTrackListLinkDef.h
	rootcint -f SvxTrackList_Dict.C -c $(DEFS) $(DEFAULT_INCLUDES) $(RINCLUDES) $^

SvxTrack_Dict.C: SvxTrack.h SvxTrack_v1.h SvxTrackFit.h SvxTrackFit_v1.h SvxTrackProj.h SvxTrackProj_v1.h SvxTrackLinkDef.h
	rootcint -f SvxTrack_Dict.C -c $(DEFS) $(DEFAULT_INCLUDES) $(RINCLUDES) $^

# Rule for generating CINT dictionaries from class headers
%Dict.C %Dict.h: %Module.h
	rootcint -f $*Dict.C -c $(DEFS) $(DEFAULT_INCLUDES) $(RINCLUDES) $^

%Dict.C %Dict.h: %.hh
	rootcint -f $*Dict.C -c $(DEFS) $(DEFAULT_INCLUDES) $(RINCLUDES) $^

# Rule for generating CINT dictionaries from class headers.
Svx_Dict.C: \
  SvxCommon.h \
  SvxHit.h \
  SvxGhit.h \
  SvxGhitv1.h \
  SvxGhitList.h \
  SvxGhitListv1.h \
  SvxPacket.h \
  Svx_stripixelPacket.h \
  SvxPacketList.h \
  Svx_stripixelPacketList.h \
  SvxRawhit.h \
  SvxRawhitv1.h \
  SvxRawhitv2.h \
  SvxRawhitv3.h \
  SvxRawhitv4.h \
  SvxRawhitv5.h \
  SvxRawhitList.h \
  SvxRawhitListv1.h \
  SvxRawhitListv2.h \
  SvxRawhitListv3.h \
  SvxRawhitListv4.h \
  SvxRawhitListv5.h \
  SvxPixelRawHitList.h \
  SvxStripRawHitList.h \
  SvxConvertPixelRawHitList.h \
  SvxConvertStripRawHitList.h \
  SvxGhitRawhit.h \
  SvxGhitRawhitv1.h \
  SvxGhitRawhitList.h \
  SvxGhitRawhitListv1.h \
  SvxCluster.h \
  SvxClusterv1.h \
  SvxClusterv2.h \
  SvxClusterv3.h \
  SvxClusterv4.h \
  SvxClusterv5.h \
  SvxClusterList.h \
  SvxClusterListv1.h \
  SvxClusterListv2.h \
  SvxClusterListv3.h \
  SvxClusterListv4.h \
  SvxClusterListv5.h \
  SvxRawhitCluster.h \
  SvxRawhitClusterv1.h \
  SvxRawhitClusterList.h \
  SvxRawhitClusterListv1.h \
  SvxGhitCluster.h \
  SvxGhitClusterv1.h \
  SvxGhitClusterList.h \
  SvxGhitClusterListv1.h \
  SvxSegment.h \
  SvxSegmentv1.h \
  SvxSegmentv2.h \
  SvxSegmentv3.h \
  SvxSegmentv4.h \
  SvxSegmentv5.h \
  SvxSegmentv6.h \
  SvxSegmentList.h \
  SvxSegmentListv1.h \
  SvxSegmentListv2.h \
  SvxSegmentListv3.h \
  SvxSegmentListv4.h \
  SvxSegmentListv5.h \
  SvxSegmentListv6.h \
  SvxEventInfo.h \
  SvxEventInfov1.h \
  SvxEventInfov2.h \
  SvxEventInfov3.h \
  SvxEventInfov4.h \
  SvxClusterInfo.h \
  SvxClusterInfov1.h \
  SvxClusterInfov2.h \
  SvxClusterInfov3.h \
  SvxClusterInfov4.h \
  SvxClusterInfov5.h \
  SvxResidualInfo.h \
  SvxResidualInfov1.h \
  SvxCentralTrack.h \
  SvxCentralTrackv1.h \
  SvxCentralTrackv2.h \
  SvxCentralTrackv3.h \
  SvxCentralTrackv4.h \
  SvxCentralTrackv5.h \
  SvxCentralTrackv6.h \
  SvxCentralTrackv7.h \
  SvxCentralTrackv8.h \
  SvxCentralTrackv9.h \
  SvxCentralTrackList.h \
  SvxCentralTrackListv1.h \
  SvxCentralTrackListv2.h \
  SvxCentralTrackListv3.h \
  SvxCentralTrackListv4.h \
  SvxCentralTrackListv5.h \
  SvxCentralTrackListv6.h \
  SvxCentralTrackListv7.h \
  SvxCentralTrackListv8.h \
  SvxCentralTrackListv9.h \
  SvxDeadMap.h \
  SvxStripThreshold.h \
  SvxPixelHotDeadMap.h \
  SvxPixelHotDeadMapv2.h \
  SvxBeamCenterPar.h \
  SvxDaqErrorMap.h \
  SvxFindHotDead.h \
  SvxReco.h \
  SvxSimulator.h \
  SvxEncode.h \
  SvxDecode.h \
  SvxParManager.h  \
  SvxApplyHotDead.h  \
  SvxMergeRawHits.h  \
  SvxSelectClusters.h  \
  SvxPixStructure.h \
  SvxPixStruct.h \
  SvxSensor.h \
  SvxStripixel.h \
  SvxPixel1v1.h \
  SvxStrip11v1.h \
  SvxStrip11v2.h \
  SvxVertexReco.h \
  SvxStandAloneReco.h \
  SvxStandAloneRecoLowMult.h \
  SvxCglReco.h \
  SvxMap.h \
  SvxClusterContainer.h \
  SvxMasterReco.h \
  SvxCentralTrackReco.h \
  SvxPriVertexSeedFinder.h \
  SvxPrimVertexFinder.h \
  SvxPrimVertexFinderLowMult.h \
  SvxPrimVertexInfo.h \
  SvxPrimVertexInfov1.h \
  SvxStripFindHotDead.h \
  SvxEventKiller.h \
  SvxVertexCombiner.h \
  SvxERecal.h \
  SvxComponentGeom.h \
  SvxLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(RINCLUDES) $^

clean-local:
	rm -f *Dict*
