AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = librpcoo_interface.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CXXFLAGS = -Wall -Werror

librpcoo_interface_la_LIBADD = \
  -lrpcoo_base \
  -lmutoo_base \
  -lrpcgeom
 
noinst_HEADERS = 

INCLUDES = -I$(GSL_MAIN)/include -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  TRpcClusCentroid.h \
  TRpcClusCentroid_v1.h \
  TRpcClus.h \
  TRpcClusMap.h \
  TRpcClus_v1.h \
  TRpcCoord.h \
  TRpcCoordMap.h \
  TRpcCoord_v1.h \
  TRpcHit.h \
  TRpcHitMap.h \
  TRpcHit_v1.h \
  TRpcHodoHit.h \
  TRpcHodoHitMap.h \
  TRpcHodoHit_v1.h \
  TRpcMCHit.h \
  TRpcMCHitMap.h \
  TRpcMCHit_v1.h \
  TRpcMCStrip.h \
  TRpcMCStrip_v1.h \
  TRpcMuoTrk.h \
  TRpcMuoTrkMap.h \
  TRpcMuoTrk_v1.h \
  TRpcRoad.h \
  TRpcRoadMap.h \
  TRpcRoad_v1.h \
  TRpcTrk.h \
  TRpcTrkMap.h \
  TRpcTrk_v1.h \
  TRpcTrk_v2.h \
  TRpcTrk_v3.h

##############################################
# please add new classes in alphabetical order

librpcoo_interface_la_SOURCES = \
  TRpcClusCentroid.cxx \
  TRpcClusCentroid_v1.cxx \
  TRpcClusCentroidDict.C \
  TRpcClusCentroid_v1Dict.C \
  TRpcClus.cxx \
  TRpcClusDict.C \
  TRpcClusMap.cxx \
  TRpcClus_v1.cxx \
  TRpcClus_v1Dict.C \
  TRpcCoord.cxx \
  TRpcCoordDict.C \
  TRpcCoordMap.cxx \
  TRpcCoord_v1.cxx \
  TRpcCoord_v1Dict.C \
  TRpcHit.cxx \
  TRpcHitDict.C \
  TRpcHitMap.cxx \
  TRpcHit_v1.cxx \
  TRpcHit_v1Dict.C \
  TRpcHodoHit.cxx \
  TRpcHodoHitDict.C \
  TRpcHodoHitMap.cxx \
  TRpcHodoHit_v1.cxx \
  TRpcHodoHit_v1Dict.C \
  TRpcMCHit.cxx \
  TRpcMCHitDict.C \
  TRpcMCHitMap.cxx \
  TRpcMCHit_v1.cxx \
  TRpcMCHit_v1Dict.C \
  TRpcMCStrip.cxx \
  TRpcMCStripDict.C \
  TRpcMCStrip_v1.cxx \
  TRpcMCStrip_v1Dict.C \
  TRpcMuoTrk.cxx \
  TRpcMuoTrkDict.C \
  TRpcMuoTrkMap.cxx \
  TRpcMuoTrk_v1.cxx \
  TRpcMuoTrk_v1Dict.C \
  TRpcRoad.cxx \
  TRpcRoadDict.C \
  TRpcRoadMap.cxx \
  TRpcRoad_v1.cxx \
  TRpcRoad_v1Dict.C \
  TRpcTrk.cxx \
  TRpcTrkDict.C \
  TRpcTrkMap.cxx \
  TRpcTrk_v1.cxx \
  TRpcTrk_v1Dict.C \
  TRpcTrk_v2.cxx \
  TRpcTrk_v2Dict.C \
  TRpcTrk_v3.cxx \
  TRpcTrk_v3Dict.C

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  librpcoo_interface.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

### general root dictionary generation
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
