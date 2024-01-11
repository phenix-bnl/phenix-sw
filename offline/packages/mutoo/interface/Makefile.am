AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libmutoo_interface.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmutoo_interface_la_LIBADD = \
  -lmutoo_base
noinst_HEADERS = 

INCLUDES = -I$(GSL_MAIN)/include -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  TMCPrimary.hh \
  TMCPrimary_v1.hh \
  TMCPrimary_v2.hh \
  TMCPrimary_v3.hh \
  TMCPrimaryMap.h \
  TMutClus.hh \
  TMutClus_v1.hh\
  TMutClus_v2.hh\
  TMutClus_v3.hh\
  TMutClusCentroid.hh \
  TMutClusCentroid_v1.hh \
  TMutClusMap.h \
  TMutCoord.hh\
  TMutCoord_v1.hh \
  TMutCoord_v2.hh \
  TMutCoord_v3.hh \
  TMutCoordMap.h\
  TMutEval_v1.hh\
  TMutEvalMap.h \
  TMutEvalRes.hh\
  TMutGapCoord.hh \
  TMutGapCoord_v1.hh \
  TMutGapCoord_v2.hh \
  TMutGapCoordMap.h \
  TMutHit.hh \
  TMutHit_v1.hh \
  TMutHit_v2.hh \
  TMutHitMap.h\
  TMutMCHit.hh\
  TMutMCHit_v1.hh \
  TMutMCHit_v2.hh \
  TMutMCHitMap.h\
  TMutMCStrip.hh\
  TMutMCTrk.hh\
  TMutMCTrk_v1.hh \
  TMutMCTrk_v2.hh \
  TMutMCTrk_v3.hh \
  TMutMCTrk_v4.hh \
  TMutMCTrk_v5.hh \
  TMutMCTrkMap.h\
  TMutStub.hh \
  TMutStub_v1.hh\
  TMutStubMap.h \
  TMutTrk.hh \
  TMutTrk_v1.hh \
  TMutTrk_v2.hh \
  TMutTrk_v3.hh \
  TMutTrk_v4.hh \
  TMutTrkEval.hh\
  TMutTrkMap.h\
  TMutTrkRes.hh \
  TMutVtx.hh \
  TMutVtx_v1.hh \
  TMutVtx_v2.hh \
  TMutVtxMap.h 

##############################################
# please add new classes in alphabetical order

libmutoo_interface_la_SOURCES = \
  TMCPrimary.cxx\
  TMCPrimaryDict.C\
  TMCPrimary_v1.cxx \
  TMCPrimary_v1Dict.C \
  TMCPrimary_v2.cxx \
  TMCPrimary_v2Dict.C \
  TMCPrimary_v3.cxx \
  TMCPrimary_v3Dict.C \
  TMCPrimaryMap.cxx \
  TMutClus.cxx \
  TMutClusDict.C\
  TMutClus_v1.cxx \
  TMutClus_v1Dict.C \
  TMutClus_v2.cxx \
  TMutClus_v2Dict.C \
  TMutClus_v3.cxx \
  TMutClus_v3Dict.C \
  TMutClusCentroid.cxx \
  TMutClusCentroidDict.C\
  TMutClusCentroid_v1.cxx \
  TMutClusCentroid_v1Dict.C \
  TMutClusMap.cxx \
  TMutCoord.cxx \
  TMutCoordDict.C \
  TMutCoord_v1.cxx\
  TMutCoord_v1Dict.C \
  TMutCoord_v2.cxx\
  TMutCoord_v2Dict.C \
  TMutCoord_v3.cxx\
  TMutCoord_v3Dict.C \
  TMutCoordMap.cxx\
  TMutEval_v1.cxx \
  TMutEval_v1Dict.C \
  TMutEvalRes.cxx \
  TMutEvalResDict.C \
  TMutEvalMap.cxx \
  TMutGapCoord.cxx\
  TMutGapCoordDict.C \
  TMutGapCoord_v1.cxx \
  TMutGapCoord_v1Dict.C \
  TMutGapCoord_v2.cxx \
  TMutGapCoord_v2Dict.C \
  TMutGapCoordMap.cxx \
  TMutHit.cxx \
  TMutHitDict.C \
  TMutHit_v1.cxx\
  TMutHit_v1Dict.C\
  TMutHit_v2.cxx\
  TMutHit_v2Dict.C\
  TMutHitMap.cxx\
  TMutMCHit.cxx \
  TMutMCHitDict.C \
  TMutMCHit_v1.cxx\
  TMutMCHit_v1Dict.C \
  TMutMCHit_v2.cxx\
  TMutMCHit_v2Dict.C \
  TMutMCHitMap.cxx\
  TMutMCStrip.cxx \
  TMutMCStripDict.C \
  TMutMCTrk.cxx \
  TMutMCTrkDict.C \
  TMutMCTrk_v1.cxx\
  TMutMCTrk_v1Dict.C \
  TMutMCTrk_v2.cxx\
  TMutMCTrk_v2Dict.C \
  TMutMCTrk_v3.cxx\
  TMutMCTrk_v3Dict.C \
  TMutMCTrk_v4.cxx\
  TMutMCTrk_v4Dict.C \
  TMutMCTrk_v5.cxx\
  TMutMCTrk_v5Dict.C \
  TMutMCTrkMap.cxx\
  TMutStub.cxx \
  TMutStubDict.C\
  TMutStub_v1.cxx \
  TMutStub_v1Dict.C \
  TMutStubMap.cxx \
  TMutTrk.cxx \
  TMutTrkDict.C \
  TMutTrk_v1.cxx\
  TMutTrk_v1Dict.C\
  TMutTrk_v2.cxx\
  TMutTrk_v2Dict.C\
  TMutTrk_v3.cxx\
  TMutTrk_v3Dict.C\
  TMutTrk_v4.cxx\
  TMutTrk_v4Dict.C\
  TMutTrkEval.cxx \
  TMutTrkEvalDict.C \
  TMutTrkRes.cxx\
  TMutTrkResDict.C\
  TMutTrkMap.cxx\
  TMutVtx.cxx \
  TMutVtxDict.C \
  TMutVtx_v1.cxx\
  TMutVtx_v1Dict.C\
  TMutVtx_v2.cxx\
  TMutVtx_v2Dict.C\
  TMutVtxMap.cxx 

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libmutoo_interface.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

### general root dictionary generation (including LinkDef)
%Dict.C: %.hh %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

### root dictionary generation for files with no LinkDef
%Dict.C: %.hh
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
