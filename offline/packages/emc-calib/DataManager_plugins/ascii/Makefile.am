AUTOMAKE_OPTIONS = foreign 

BUILT_SOURCES = \
  testexternals.C

INCLUDES =  \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = libemcOMascii.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libemcOMascii_la_LIBADD = \
  -lemcCalib \
  -lemcCalib2 \
  -lemcDataManager \
  -lemcOMbase \
  -lEmcStatic \
  -lpdbcalBase \
  -lphool \
  -lphgeo

libemcOMascii_la_SOURCES = \
  asciitimestamp.C \
  dirfilemanip.C \
  emcOMCalibrationDataIniCal.C \
  emcOMCalibrationDataTofSectorOffset.C \
  emcOMGains.C \
  emcOMHLRatioFEM.C \
  emcOMHLRatios.C \
  emcOMIniCalConstruction.C \
  emcOMLCTofFEM.C \
  emcOMLCTofs.C \
  emcOMPedestalFEM.C \
  emcOMPedestals5.C \
  emcOMQAFEM.C \
  emcOMQAs.C \
  emcOMRejectList.C \
  emcOMTracedFEM.C \
  emcOMWalkTofFEM.C \
  emcOMWalkTofs.C \
  RunTimesAscii.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libemcOMascii.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


testexternals_SOURCES = testexternals.C
