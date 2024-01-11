AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libLPol.la   

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libLPol_la_LIBADD = \
  -lphool \
  -lpdbcalBase \
  -lphflag \
  -lSubsysReco

noinst_HEADERS = \
  LPolRawLinkDef.h 

include_HEADERS = \
  LPol.hh \
  LPolEvent.hh \
  LPolRaw.h LPolRawv1.h \
  LPolRawHit.h LPolRawHitv1.h \
  LPolConst.h

libLPol_la_SOURCES = \
  LPolDict.C \
  LPolEvent.cc \
  LPolMap.C \
  LPolRaw.C LPolRawv1.C \
  LPolRawHit.C LPolRawHitv1.C \
  LPolRawDict.C \
  LPolReco.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libLPol.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

LPolRawDict.C : LPolRaw.h LPolRawv1.h LPolRawHit.h LPolRawHitv1.h LPolRawLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

LPolDict.C : \
  LPolMap.h \
  LPolReco.h \
  LPolLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
