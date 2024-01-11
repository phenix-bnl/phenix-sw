AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = liblpc.la   

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

liblpc_la_LIBADD = \
  -lphool \
  -lpdbcalBase \
  -lphflag \
  -lSubsysReco

noinst_HEADERS = \
  lpcRawLinkDef.h 

include_HEADERS = \
  lpc.hh \
  lpcEvent.hh \
  lpcReturncodes.h \
  lpcRaw.h lpcRawv1.h lpcRawv2.h \
  lpcRawHit.h lpcRawHitv1.h lpcRawHitv2.h 

liblpc_la_SOURCES = \
  lpcDict.C \
  lpcEvent.cc \
  lpcMap.C \
  lpcRaw.C lpcRawv1.C lpcRawv2.C \
  lpcRawHit.C lpcRawHitv1.C lpcRawHitv2.C \
  lpcRawDict.C \
  lpcReco.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  liblpc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

lpcRawDict.C : lpcRaw.h lpcRawv1.h lpcRawv2.h lpcRawHit.h lpcRawHitv1.h lpcRawHitv2.h lpcRawLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

lpcDict.C : \
  lpcMap.h \
  lpcReco.h \
  lpcLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
