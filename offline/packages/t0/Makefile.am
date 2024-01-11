AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libt0.la   

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libt0_la_LIBADD = -lphool 

noinst_HEADERS = \
  T0OutLinkDef.h

include_HEADERS = \
  T0Out.h \
  T0Outv1.h \
  T0Outv2.h \
  T0Outv3.h \
  T0Returncodes.h

libt0_la_SOURCES = \
  T0OutDict.C \
  T0Out.C \
  T0Outv1.C \
  T0Outv2.C \
  T0Outv3.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libt0.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

T0OutDict.C : T0Out.h T0Outv1.h T0Outv2.h T0Outv3.h T0OutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	 rm -f *Dict.*
