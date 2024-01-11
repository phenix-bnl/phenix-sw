AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libhbd.la 

noinst_HEADERS = \
  Hbd_Dict.h \
  HbdLinkDef.h \
  HbdNeighbours.h \
  HbdNeighboursV2.h \
  HbdCutterThresholdParameters.h \
  hit_pad_list.h \
  pad_area.h

include_HEADERS = \
  HbdBlob.h \
  HbdBlobv1.h \
  HbdBlobv2.h \
  HbdBlobList.h \
  HbdBlobListv1.h \
  HbdBlobListv2.h \
  HbdRaw.h \
  HbdRawv1.h \
  HbdRawList.h \
  HbdRawListv1.h \
  HbdCell.h \
  HbdCellv1.h \
  HbdCellv2.h \
  HbdCellList.h \
  HbdCellListv1.h \
  HbdCellListv2.h \
  HbdCluster.h \
  HbdClusterList.h \
  HbdClusterizer.h \
  HbdWisClusterizer.h \
  HbdWisCluster.h \
  HbdWisClusterList.h \
  HbdMinPadClusterizer.h \
  HbdMiniCell.h \
  HbdMiniCellv1.h \
  HbdMiniCellList.h \
  HbdMiniCellListv1.h \
  HbdMiniCellListv2.h \
  HbdMiniToFullCell.h \
  HbdNeighboursV2.h \
  HbdFinalSimulator.h \
  HbdFinalSimSupport.h \
  HbdGainCalibTrkV.h \
  HbdGhit.h \
  HbdGhitv1.h \
  HbdGhitList.h \
  HbdGhitListv1.h \
  HbdHit.h \
  HbdHitv1.h \
  HbdHitList.h \
  HbdHitListv1.h \
  Hbd.h \
  HbdDcmRaw.h \
  hbdDetectorGeo.hh \
  HbdCellListAnalyzer.h \
  HbdPreClusterMaker.h \
  hbdAdcCalib.hh \
  HbdReco.h \
  HbdSimreco.h \
  HbdNeighbours.h \
  YWCutter.h \
  YWParticle.h 

libhbd_la_SOURCES = \
  HbdBlob.C \
  HbdBlobList.C \
  HbdBlobListv1.C \
  HbdBlobListv2.C \
  HbdBlobv1.C \
  HbdBlobv2.C \
  HbdRaw.C \
  HbdRawList.C \
  HbdRawListv1.C \
  HbdRawv1.C \
  HbdCell.C \
  HbdCellList.C \
  HbdCellListv1.C \
  HbdCellListv2.C \
  HbdCellv1.C \
  HbdCellv2.C \
  HbdMiniCell.C \
  HbdMiniCellList.C \
  HbdMiniCellListv1.C \
  HbdMiniCellListv2.C \
  HbdMiniCellv1.C \
  HbdMiniToFullCell.C \
  HbdGhit.C \
  HbdGhitList.C \
  HbdGhitListv1.C \
  HbdGhitv1.C \
  HbdHit.C \
  HbdHitList.C \
  HbdHitListv1.C \
  HbdHitv1.C \
  hbdDetectorGeo.cc \
  HbdCellListAnalyzer.C \
  HbdPreClusterMaker.C \
  hbdAdcCalib.cc \
  HbdFinalSimulator.C \
  HbdGainCalibTrkV.C \
  HbdClusterizer.C \
  HbdWisClusterizer.C \
  HbdMinPadClusterizer.C \
  HbdDcmRaw.C \
  HbdFinalSimSupport.C \
  HbdReco.C \
  HbdSimreco.C \
  YWCutter.C \
  YWParticle.C

nodist_libhbd_la_SOURCES = Hbd_Dict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libhbd_la_LIBADD = \
  -lphgeo \
  -lfun4all \
  -lgea \
  -lheader \
  -lnanoDST \
  -lpdbcalBase \
  -lphflag \
  -lphool \
  -lphtime \
  -lPISARoot \
  -lSubsysReco \
  -lWrappers \
  -ltrigger

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libhbd.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
Hbd_Dict.h: Hbd_Dict.C
Hbd_Dict.C: \
  HbdBlob.h \
  HbdBlobList.h \
  HbdBlobListv1.h \
  HbdBlobListv2.h \
  HbdBlobv1.h \
  HbdBlobv2.h \
  HbdRaw.h \
  HbdRawList.h \
  HbdRawListv1.h \
  HbdRawv1.h \
  HbdCell.h \
  HbdCellList.h \
  HbdCellListv1.h \
  HbdCellListv2.h \
  HbdCellv1.h \
  HbdCellv2.h \
  HbdCluster.h \
  HbdClusterList.h \
  HbdMiniCell.h \
  HbdMiniCellList.h \
  HbdMiniCellListv1.h \
  HbdMiniCellListv2.h \
  HbdMiniCellv1.h \
  HbdMiniToFullCell.h \
  HbdGhit.h \
  HbdGhitList.h \
  HbdGhitListv1.h \
  HbdGhitv1.h \
  HbdHit.h \
  HbdHitList.h \
  HbdHitListv1.h \
  HbdHitv1.h \
  hbdDetectorGeo.hh \
  HbdCellListAnalyzer.h \
  HbdPreClusterMaker.h \
  hbdAdcCalib.hh \
  HbdFinalSimulator.h \
  HbdGainCalibTrkV.h \
  HbdClusterizer.h \
  HbdDcmRaw.h \
  HbdReco.h \
  HbdSimreco.h \
  HbdWisClusterizer.h \
  HbdWisClusterList.h \
  HbdWisCluster.h \
  HbdCluster.h \
  HbdMinPadClusterizer.h \
  YWCutter.h \
  YWParticle.h \
  HbdLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict*


testexternals_SOURCES = testexternals.C
