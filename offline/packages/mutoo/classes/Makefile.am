
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmutoo_classes.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmutoo_classes_la_LIBADD = \
 -lmutoo_interface

include_HEADERS = \
  TMutBPUtil.h \
  TMutClusterFit.h \
  TMutClusterFitEval.h \
  TMutCoordFill.h \
  TMutErrorStats.h \
  TMutLinearModel.h \
  TMutMathieson.h \
  TMutMathiesonPar.h \
  TMutMeasureModel.h \
  TMutStraightTrackFit.h \
  TMutStubFinder.h \
  TMutTrackModel.h

libmutoo_classes_la_SOURCES = \
  TMutBPUtil.cxx \
  TMutClusterFit.cxx \
  TMutClusterFitEval.cxx \
  TMutCoordFill.cxx \
  TMutErrorStats.cxx \
  TMutLinearModel.cxx \
  TMutMathieson.cxx \
  TMutMathiesonPar.cxx \
  TMutMathiesonParDict.C \
  TMutMeasureModel.cxx \
  TMutStraightTrackFit.cxx \
  TMutStubFinder.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libmutoo_classes.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: %.hh 
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHKeyDict.C: PHKey.hh PHKeyLinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
