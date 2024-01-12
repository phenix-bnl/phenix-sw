AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

include_HEADERS =

noinst_HEADERS = \
  CombinationLinkDef.h \
  KClusterLinkDef.h \
  KEventLinkDef.h \
  KEventCentLinkDef.h \
  qa_pi0LinkDef.h

lib_LTLIBRARIES = \
  libpi0qa.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libpi0qa_la_LIBADD = \
  -lemc-datatypes \
  -lnanoDST \
  -lTHmul \
  -ltrigger

libpi0qa_la_SOURCES = \
  qa_pi0.cc \
  qa_pi0Dict.C \
  KEvent.cc \
  KEventDict.C \
  KEventCent.cc \
  KEventCentDict.C \
  KCluster.cc \
  KClusterDict.C \
  Combination.cc \
  CombinationDict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libpi0qa.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C : %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict.*


