AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libmupc.la 

noinst_HEADERS = \
  MuPCLinkDef.h 

include_HEADERS = \
  MuPCCluster.h \
  MuPCClusterv1.h \
  MuPCSnglCluster.h \
  MuPCSnglClusterv1.h \
  MuPCPar.h \
  MuPCAnalysis.h \
  uIDLL1Road.h \
  uIDLL1Roadv1.h \
  uIDLL1SnglRoad.h \
  uIDLL1SnglRoadv1.h \
  uIDLL1Analysis.h


libmupc_la_SOURCES = \
  MuPCCluster.C \
  MuPCClusterv1.C \
  MuPCSnglCluster.C \
  MuPCSnglClusterv1.C \
  MuPCAnalysis.C \
  uIDLL1Road.C \
  uIDLL1Roadv1.C \
  uIDLL1SnglRoad.C \
  uIDLL1SnglRoadv1.C \
  uIDLL1Analysis.C \
  MuPC_Dict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmupc_la_LIBADD = \
  -lphool \
  -lmutgeom \
  -lmuigeom \
  -lmuioo_interface \
  -lMuidTriggerEmulator \
  -lmutoo_base

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libmupc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
MuPC_Dict.C: \
  MuPCCluster.h \
  MuPCClusterv1.h \
  MuPCSnglCluster.h \
  MuPCSnglClusterv1.h \
  uIDLL1Road.h \
  uIDLL1Roadv1.h \
  uIDLL1SnglRoad.h \
  uIDLL1SnglRoadv1.h \
  MuPCLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*


testexternals_SOURCES = testexternals.C
