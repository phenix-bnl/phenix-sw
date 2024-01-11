
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmuioo_modules.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmuioo_modules_la_LIBADD = \
 -lmuigeom \
 -lmuioo_classes \
 -lmutoo_interface \
 -lMuidTriggerEmulator

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

include_HEADERS = \
  mMuiBLTEmulator.h \
  mMuiBLTEmulatorPar.h \
  mMuiClusterFinder.h \
  mMuiClusterFinderPar.h \
  mMuiEmbed.h \
  mMuiEmbedPar.h \
  mMuiEvalO.h \
  mMuiEvalOPar.h \
  mMuiFastRoadFinder.h \
  mMuiFastRoadFinderPar.h \
  mMuiFindRoad.h \
  mMuiFindRoadPar.h \
  mMuiFitRoadO.h \
  mMuiFitRoadOPar.h \
  mMuiPackPRDF.h \
  mMuiPackPRDFPar.h \
  mMuiRawUnpack.h \
  mMuiRawUnpackPar.h \
  mMuiResponse.h \
  mMuiResponsePar.h \
  mMuiRoadAssoc.h \
  mMuiRoadAssocPar.h \
  mMuiRoadFinder1.h \
  mMuiRoadFinder1Par.h \
  mMuiRoadFinderPass2.h \
  mMuiRoadFinderPass2Par.h \
  mMuiSlowSim.h \
  mMuiSlowSimPar.h \
  mMuiTriggerEmulator.h \
  mMuiTriggerEmulatorPar.h \
  TMuiParBase.h

libmuioo_modules_la_SOURCES = \
  mMuiBLTEmulator.cxx \
  mMuiClusterFinder.cxx \
  mMuiEmbed.cxx \
  mMuiEvalO.cxx \
  mMuiFastRoadFinder.cxx \
  mMuiFindRoad.cxx \
  mMuiFitRoadO.cxx \
  mMuiPackPRDF.cxx \
  mMuiRawUnpack.cxx \
  mMuiResponse.cxx \
  mMuiRoadAssoc.cxx \
  mMuiRoadFinder1.cxx \
  mMuiRoadFinderPass2.cxx \
  mMuiSlowSim.cxx \
  mMuiTriggerEmulator.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals
  
testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmuioo_modules.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
