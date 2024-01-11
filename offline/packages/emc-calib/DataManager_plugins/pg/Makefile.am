AUTOMAKE_OPTIONS = foreign 

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir` -I$(OPT_PHENIX)/include

lib_LTLIBRARIES = libemcOMpg.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(OPT_PHENIX)/lib

libemcOMpg_la_LIBADD = -lemcOMbase -lodbc++

libemcOMpg_la_SOURCES = \
  emcOMTimeStamp.C \
  emcOMCalibrationData.C \
  emcOMGains.C \
  emcOMGeometry.C \
  emcOMHLRatios.C \
  emcOMHLRatioFEM.C \
  emcOMLCTofFEM.C \
  emcOMLCTofs.C \
  emcOMPedestal5FEM.C \
  emcOMPedestals5.C \
  emcOMQAFEM.C \
  emcOMQAs.C \
  emcOMRejectList.C \
  emcOMTofT0FEM.C \
  emcOMTracedFEM.C \
  emcOMWalkTofFEM.C \
  emcOMWalkTofs.C \
  emcPgStorageManager.C \
  RunTimesPg.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libemcOMpg.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

testexternals_SOURCES = testexternals.C
