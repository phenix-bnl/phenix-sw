AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libfcl.la   

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libfcl_la_LIBADD = \
  -lSubsysReco \
  -lnanoDST \
  -lpdbcalBase \
  -lphflag \
  -lphool \
  -lphtime \
  -ltrigger \
  -lzdc

noinst_HEADERS = \
  FclIndexerLinkDef.h \
  FclOutLinkDef.h \
  FclRawLinkDef.h \
  FclRecalLinkDef.h

include_HEADERS = \
  FclCalib.h \
  FclCompactRawv1.h \
  FclConsts.h \
  FclIndexer.h \
  FclOut.h \
  FclOutv1.h \
  FclRaw.h \
  FclRawv1.h \
  FclRawv2.h

libfcl_la_SOURCES = \
  FclCalib.C \
  FclCompactRawv1.C \
  FclIndexer.C \
  FclIndexerDict.C \
  FclOut.C \
  FclOutv1.C \
  FclOutDict.C \
  FclRaw.C \
  FclRawDict.C \
  FclRawv1.C \
  FclRawv2.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libfcl.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

FclIndexerDict.C : FclIndexer.h FclCalib.h FclIndexerLinkDef.h
	rootcint -f $@ -c  $(DEFAULT_INCLUDES) $(INCLUDES) $^

FclRawDict.C : \
  FclRaw.h \
  FclRawv1.h \
  FclRawv2.h \
  FclCompactRawv1.h \
  FclRawLinkDef.h
	rootcint -f $@ -c  $(DEFAULT_INCLUDES) $(INCLUDES) $^

FclOutDict.C : \
  FclOut.h  \
  FclOutv1.h \
  FclOutLinkDef.h
	rootcint -f $@ -c  $(DEFAULT_INCLUDES) $(INCLUDES) $^

FclRecalDict.C : \
  FclRecal.h FclRecalLinkDef.h
	rootcint -f $@ -c  $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*


testexternals_SOURCES = testexternals.C
