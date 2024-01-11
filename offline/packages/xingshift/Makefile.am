AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = \
  libxingshift.la

libxingshift_la_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib `root-config --libs`

noinst_HEADERS = \
  TGeneralXingShiftTest.h

include_HEADERS = \
  TGeneralXingShiftInput.h \
  TBunch.h \
  TRing.h \
  TRingPattern.h \
  TSimpleXingShift.h \
  TGeneralXingShift.h \
  xingshift_chi2.h

libxingshift_la_SOURCES = \
  XingShift_Dict.C \
  TGeneralXingShiftInput.C \
  TBunch.C \
  TRing.C \
  TRingPattern.C \
  TGeneralXingShift.C \
  TSimpleXingShift.C \
  TGeneralXingShiftTest.C \
  xingshift_chi2.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libxingshift.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


XingShift_Dict.C: \
  TRingPattern.h \
  TRing.h \
  TBunch.h \
  TSimpleXingShift.h \
  TGeneralXingShift.h \
  TGeneralXingShiftInput.h \
  xingshift_chi2.h \
  XingShiftLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict.*

