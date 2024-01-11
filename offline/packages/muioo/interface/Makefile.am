AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmuioo_interface.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmuioo_interface_la_LIBADD = \
  -lmutoo_base \
  -lmuioo_base

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

include_HEADERS = \
  TMui1DRoadMapO.h \
  TMui1DRoadO.h \
  TMui1DRoadO_v1.h \
  TMuiClusterMapO.h \
  TMuiClusterO.h \
  TMuiClusterO_v1.h \
  TMuiClusterO_v2.h \
  TMuiEval.hh \
  TMuiEvalMap.h \
  TMuiHitMapO.h \
  TMuiHitO.h \
  TMuiHitO_v1.h \
  TMuiMCHitMapO.h \
  TMuiMCHitO.h \
  TMuiMCHitO_v1.h \
  TMuiPseudoBLTMapO.h \
  TMuiPseudoBLTO.h \
  TMuiPseudoLL1.h \
  TMuiPseudoLL1Map.h \
  TMuiRoadEval.hh \
  TMuiRoadMapO.h \
  TMuiRoadO.h \
  TMuiRoadO_v1.h \
  TMuiRoadO_v2.h

libmuioo_interface_la_SOURCES = \
  TMui1DRoadMapO.cxx \
  TMui1DRoadO.cxx \
  TMui1DRoadO_Dict.C \
  TMui1DRoadO_v1.cxx \
  TMui1DRoadO_v1_Dict.C \
  TMuiClusterMapO.cxx \
  TMuiClusterO.cxx \
  TMuiClusterO_Dict.C \
  TMuiClusterO_v1.cxx \
  TMuiClusterO_v1_Dict.C \
  TMuiClusterO_v2.cxx \
  TMuiClusterO_v2_Dict.C \
  TMuiEval.cxx \
  TMuiEval_Dict.C \
  TMuiEvalMap.cxx \
  TMuiHitMapO.cxx \
  TMuiHitO.cxx \
  TMuiHitO_Dict.C \
  TMuiHitO_v1.cxx \
  TMuiHitO_v1_Dict.C \
  TMuiMCHitMapO.cxx \
  TMuiMCHitO.cxx \
  TMuiMCHitO_Dict.C \
  TMuiMCHitO_v1.cxx \
  TMuiMCHitO_v1_Dict.C \
  TMuiPseudoBLTMapO.cxx \
  TMuiPseudoBLTO.cxx \
  TMuiPseudoBLTO_Dict.C \
  TMuiPseudoLL1.cxx \
  TMuiPseudoLL1_Dict.C \
  TMuiPseudoLL1Map.cxx \
  TMuiRoadEval.cxx \
  TMuiRoadEval_Dict.C \
  TMuiRoadMapO.cxx \
  TMuiRoadO.cxx \
  TMuiRoadO_Dict.C \
  TMuiRoadO_v1.cxx \
  TMuiRoadO_v1_Dict.C \
  TMuiRoadO_v2.cxx \
  TMuiRoadO_v2_Dict.C

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libmuioo_interface.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%_Dict.C: %.hh %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
