AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libzdc.la

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libzdc_la_LIBADD = \
  -lRootmessage \
  -lWrappers \
  -lpdbcalBase \
  -lphool \
  -lphtime \
  -lphflag \
  -lPISARoot \
  -lSubsysReco

noinst_HEADERS = \
  ZdcReco.h \
  libzdc_LinkDef.h

include_HEADERS = \
  dZdcOutWrapper.h \
  dZdcOut.h \
  ZdcCalibPar.hh \
  ZdcCalib.hh \
  Zdc.hh \
  ZdcEvent.hh \
  ZdcGetDCM.h \
  ZdcResponse.h \
  ZdcReturncodes.h \
  ZdcGeaHits.h ZdcGeaHitsv1.h \
  ZdcRaw.h ZdcRawv1.h ZdcRawv2.h \
  ZdcDCM.h ZdcDCMv1.h \
  ZdcRawHit.h \
  ZdcHit.h \
  SmdHit.h \
  ZdcOut.h ZdcOutv1.h ZdcOutv2.h \
  ZdcNorthSouth.h \
  SmdOut.h SmdOutv1.h \
  SmdNorthSouth.h \
  ZdcCalOverflow.h \
  ZdcCalPedestal.h \
  ZdcCalEnergy.h \
  ZdcCalSlew.h \
  ZdcCalTiming.h \
  ZdcMap.h

libzdc_la_SOURCES = \
  dZdcOutWrapper.C \
  ZdcCalibPar.cc \
  ZdcCalib.cc \
  ZdcEvent.cc \
  ZdcGetDCM.cc \
  ZdcResponse.cc \
  ZdcGeaHits.C ZdcGeaHitsv1.C \
  ZdcRaw.C ZdcRawv1.C ZdcRawv2.C \
  ZdcRawHit.C \
  ZdcDCM.C ZdcDCMv1.C \
  ZdcHit.C \
  SmdHit.C \
  ZdcOut.C ZdcOutv1.C ZdcOutv2.C \
  SmdOut.C SmdOutv1.C \
  ZdcNorthSouth.C \
  SmdNorthSouth.C \
  ZdcCalOverflow.cc \
  ZdcCalPedestal.cc \
  ZdcCalEnergy.cc \
  ZdcCalSlew.cc \
  ZdcCalTiming.cc \
  ZdcMap.C \
  ZdcReco.C

nodist_libzdc_la_SOURCES = libzdc_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libzdc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libzdc_Dict.C: \
  dZdcOutWrapper.h \
  SmdHit.h \
  SmdNorthSouth.h \
  SmdOut.h \
  SmdOutv1.h \
  ZdcCalEnergy.h \
  ZdcCalib.hh \
  ZdcCalibPar.hh \
  ZdcCalOverflow.h \
  ZdcCalPedestal.h \
  ZdcCalSlew.h \
  ZdcCalTiming.h \
  ZdcDCM.h \
  ZdcDCMv1.h \
  ZdcEvent.hh \
  ZdcGeaHits.h \
  ZdcGeaHitsv1.h \
  ZdcHit.h \
  ZdcMap.h \
  ZdcNorthSouth.h \
  ZdcOut.h \
  ZdcOutv1.h \
  ZdcOutv2.h \
  ZdcRaw.h \
  ZdcRawHit.h \
  ZdcRawv1.h \
  ZdcRawv2.h \
  ZdcReco.h \
  libzdc_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict*
