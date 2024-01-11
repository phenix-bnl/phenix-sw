AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include \
	-I$(OPT_PHENIX)/include

lib_LTLIBRARIES = libuspin.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(OPT_PHENIX)/lib

libuspin_la_LIBADD = \
  -lheader \
  -lphool \
  -ltrigger \
  -lodbc++

include_HEADERS = \
  utiRunToFill.hh \
  utiBunchNumber.hh \
  utiPolarization.hh \
  utiBunchFitAsym.hh \
  utiBunchFitLumi.hh \
  utiBunchShuffleLumi.hh \
  SpinDBContent.hh \
  SpinDBInput.hh \
  SpinDBOutput.hh

noinst_HEADERS =  \
  SpinDBUtilLinkDef.h \
  utiBunchFitAsymLinkDef.h \
  utiBunchFitLumiLinkDef.h \
  utiBunchShuffleLumiLinkDef.h \
  utiRunToFillLinkDef.h

libuspin_la_SOURCES = \
  utiRunToFill.C \
  utiBunchNumber.C \
  utiPolarization.C \
  utiBunchFitAsym.C \
  utiBunchFitLumi.C \
  utiBunchShuffleLumi.C \
  SpinDBContent.cc \
  SpinDBInput.cc \
  SpinDBOutput.cc

nodist_libuspin_la_SOURCES = \
  utiRunToFill_Dict.C \
  utiBunchFitAsym_Dict.C \
  utiBunchFitLumi_Dict.C \
  utiBunchShuffleLumi_Dict.C \
  SpinDBUtil_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libuspin.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

utiRunToFill_Dict.C: utiRunToFill.hh utiRunToFillLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

utiBunchFitAsym_Dict.C: utiBunchFitAsym.hh utiBunchFitAsymLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

utiBunchFitLumi_Dict.C: utiBunchFitLumi.hh utiBunchFitLumiLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

utiBunchShuffleLumi_Dict.C: utiBunchShuffleLumi.hh utiBunchShuffleLumiLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

utiUspin_Dict.C: utiBunchNumber.hh utiPolarization.hh utiRunToFill.hh \
	utiTriggerTrackByTrack.hh utiUspinLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(CINTINCLUDES) $^

SpinDBUtil_Dict.C : SpinDBContent.hh SpinDBInput.hh SpinDBOutput.hh \
	SpinDBUtilLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
