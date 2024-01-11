AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libtofw.la 

noinst_HEADERS = \
  TofwClassLinkDef.h

include_HEADERS = \
  TofwPar.h \
  TofwCalib.h \
  TofwGeometry.h \
  TofwSnglRaw.h \
  TofwRaw.h \
  TofwSnglHit.h \
  TofwHit.h \
  TofwSnglRawv1.h \
  TofwRawv1.h \
  TofwSnglHitv1.h \
  TofwHitv1.h \
  TofwEvent.h 

libtofw_la_SOURCES = \
  TfwSimreco.C \
  TofwGeometry.C \
  TofwCalib.C \
  TofwSnglRaw.C \
  TofwSnglRawv1.C \
  TofwRaw.C \
  TofwRawv1.C \
  TofwSnglHit.C \
  TofwSnglHitv1.C \
  TofwHit.C \
  TofwHitv1.C \
  TofwEvent.C \
  TofwReco.C \
  TofwSimEvent.C \
  TofwClass_Dict.C 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libtofw_la_LIBADD = \
  -lgea \
  -lheader \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -lSubsysReco \
  -lWrappers

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libtofw.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

TofwClass_Dict.C: \
  TfwSimreco.h \
  TofwSnglRaw.h \
  TofwSnglRawv1.h \
  TofwRaw.h \
  TofwRawv1.h \
  TofwSnglHit.h \
  TofwSnglHitv1.h \
  TofwHit.h \
  TofwHitv1.h \
  TofwCalib.h \
  TofwReco.h \
  TofwClassLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict*
