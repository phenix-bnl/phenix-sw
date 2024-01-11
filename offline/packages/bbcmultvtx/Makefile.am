AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libBbcMultipleVtx.la

include_HEADERS = \
	BbcMultipleVtxCluster.hh \
	BbcMultipleVtxCluster_v1.hh \
	BbcMultipleVtxPoint.hh \
	BbcMultipleVtxPoint_v1.hh \
	BbcMultipleVtxList.hh \
	BbcMultipleVtxList_v1.hh \
	BbcMultipleVtxList_v2.hh \
	BbcMultipleVtx.hh \
	BbcMultipleVtx_v1.hh \
	BbcMultipleVtx_v2.hh \
	BbcMultipleVtxReco.hh \
	BbcMultipleVtxChecker.hh

noinst_HEADERS = \
	BbcMultipleVtxLinkDef.h

libBbcMultipleVtx_la_SOURCES = \
	BbcMultipleVtxCluster.cc \
	BbcMultipleVtxCluster_v1.cc \
	BbcMultipleVtxPoint.cc \
	BbcMultipleVtxPoint_v1.cc \
	BbcMultipleVtxList.cc \
	BbcMultipleVtxList_v1.cc \
	BbcMultipleVtxList_v2.cc \
	BbcMultipleVtx.cc \
	BbcMultipleVtx_v1.cc \
	BbcMultipleVtx_v2.cc \
	BbcMultipleVtxReco.cc \
	BbcMultipleVtxChecker.cc \
	BbcMultipleVtx_Dict.C

libBbcMultipleVtx_la_LIBADD = \
	-L$(libdir) \
	-L$(OFFLINE_MAIN)/lib \
	-lheader -lbbc

BbcMultipleVtx_Dict.C: \
	BbcMultipleVtxCluster.hh \
	BbcMultipleVtxCluster_v1.hh \
	BbcMultipleVtxPoint.hh \
	BbcMultipleVtxPoint_v1.hh \
	BbcMultipleVtxList.hh \
	BbcMultipleVtxList_v1.hh \
	BbcMultipleVtxList_v2.hh \
	BbcMultipleVtx.hh \
	BbcMultipleVtx_v1.hh \
	BbcMultipleVtx_v2.hh \
	BbcMultipleVtxReco.hh \
	BbcMultipleVtxChecker.hh \
	BbcMultipleVtxLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
