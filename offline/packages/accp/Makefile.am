AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libaccp.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libaccp_la_LIBADD = -lphool

noinst_HEADERS = \
  AccpEventLinkDef.h \
  AccpRawLinkDef.h 

include_HEADERS = \
  AccpEvent.h \
  Accp.h  \
  AccpRaw.h \
  AccpRawv1.h 

libaccp_la_SOURCES = \
  AccpEvent.C \
  AccpRaw.C     \
  AccpRawv1.C   \
  AccpRawDict.C \
  AccpEventDict.C 

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libaccp.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

AccpRawDict.C : AccpRaw.h AccpRawv1.h AccpRawLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

AccpEventDict.C : AccpEvent.h AccpEventLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

