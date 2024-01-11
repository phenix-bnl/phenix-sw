AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libtecpid.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libtecpid_la_LIBADD = \
  -ltec \
  -lcgl \
  -lcrk \
  -ldch \
  -lheader \
  -lpad \
  -lphool \
  -lgsl \
  -lphflag \
  -lphgeo 


noinst_HEADERS = \
  TecPidLinkDef.h

include_HEADERS = \
  mTecPIDModule.h \
  mTecTrackEvalModule.h \
  mTecCglModule.h \
  TecOutV5.hh \
  TecOutV6.hh \
  TecOutV7.hh \
  TecTrackHit.hh \
  TecTrackTRv3.hh \
  TecTrackV2.hh

libtecpid_la_SOURCES = \
  mTecPIDModule.cc \
  mTecTrackEvalModule.cc \
  mTecCglModule.cc \
  TecPidDict.C \
  TecOutV5.cc \
  TecOutV6.cc \
  TecOutV7.cc \
  TecTrackHit.cc \
  TecTrackTRv3.cc \
  TecTrackV2.cc \
  TTECEventDisplay.cc \
  TTECEventIterator.cc \
  TTECEventViewer.cc \
  TTECSectorSideEvent.cc

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libtecpid.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

TecPidDict.C : \
  TecOutV5.hh \
  TecOutV6.hh \
  TecOutV7.hh \
  TecTrackHit.hh \
  TecTrackTRv3.hh \
  TecTrackV2.hh \
  TTECEventDisplay.hh \
  TTECEventIterator.hh \
  TTECEventViewer.hh
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict*
