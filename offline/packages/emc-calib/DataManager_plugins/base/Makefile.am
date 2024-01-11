AUTOMAKE_OPTIONS = foreign 

INCLUDES =  \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I`root-config --incdir`

lib_LTLIBRARIES = libemcOMbase.la

bin_PROGRAMS = emcDB
emcDB_SOURCES = emcDB_main.C emcDB.C convertNormalization.C
emcDB_LDADD = libemcOMbase.la

libemcOMbase_la_SOURCES = \
  RunTimes.C \
  RunTimesFactory.C

libemcOMbase_la_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib `root-config --libs`
libemcOMbase_la_LIBADD = \
  -lemcCalib2 \
  -lemcCalib \
  -lpdbcalBase \
  -lEmcStatic \
  -lEmcDynamic \
  -lemcDataManager \
  -lphool \
  -lphtime

include_HEADERS = \
  emcOMCalFEMT.h \
  emcOMCalibrationDataT.h \
  emcOMFEMtupleT.h \
  emcOMGeometryT.h \
  emcOMHelper.h \
  emcOMHLRatioFEMT.h \
  emcOMLCTofFEMT.h \
  emcOMPedestalFEMT.h\
  emcOMPedestal5FEMT.h \
  emcOMQAFEMT.h \
  emcOMRejectListT.h \
  emcOMTofT0FEMT.h \
  emcOMTracedFEMT.h \
  emcOMWalkTofFEMT.h \
  RunTimes.h \
  RunTimesFactory.h

noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C
testexternals.C:
	@echo "int main() { }" > $@
