AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include  

lib_LTLIBRARIES = libCrkPID.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libCrkPID_la_LIBADD = \
  -lWrappers \
  -lcrk \
  -lcgl \
  -ldch \
  -lemc \
  -lgsl \
  -lpad \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -lt0

noinst_HEADERS = \
  CrkSnglRingMicrov1.h \
  CrkSnglRingMicrov2.h \
  CrkSnglRingMicrov3.h \
  CrkRingLinkDef.h 


include_HEADERS = \
  CrkPID.hh \
  CrkRing.h \
  CrkRingMicrov3.h \
  CrkProj.h \
  CrkProjv1.h

libCrkPID_la_SOURCES = \
  CrkPID.cc \
  CrkRing.C \
  CrkRingMicrov1.C \
  CrkSnglRingMicrov1.C \
  CrkRingMicrov2.C \
  CrkSnglRingMicrov2.C \
  CrkRingMicrov3.C \
  CrkSnglRingMicrov3.C \
  CrkRingMicrov4.C \
  CrkProj.C \
  CrkProjv1.C \
  CrkSnglProjv1.C \
  CrkRing_Dict.C \
  RingEmbedreco.C \
  RingReco.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libCrkPID.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

CrkRing_Dict.C: \
  CrkRing.h \
  CrkRingMicrov1.h CrkSnglRingMicrov1.h \
  CrkRingMicrov2.h CrkSnglRingMicrov2.h \
  CrkRingMicrov3.h CrkSnglRingMicrov3.h \
  CrkRingMicrov4.h \
  CrkProj.h \
  CrkProjv1.h CrkSnglProjv1.h \
  RingEmbedreco.h \
  RingReco.h \
  CrkRingLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*



testexternals_SOURCES = testexternals.C
