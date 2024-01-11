AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libdgo.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libdgo_la_LIBADD = \
  -lpdbcalBase \
  -lphflag \
  -lphgeo \
  -lphool \
  -lphtime 

libdgo_la_SOURCES = \
  PHAddressObject.C \
  PHGeometryObject.C \
  PHCalibrationObject.C \
  PHReconstructionObject.C \
  PHDchAddressObject.C \
  PHDchGeometryObject.C \
  PHDchCalibrationObject.C \
  PHDchReconstructionObject.C \
  DchBasicCalibration.C \
  PHNoiseObject.C \
  PHDchNoiseObject.C \
  DchBasicNoise.C  \
  DchBasicReconstruction.C  \
  dgoDict.C

include_HEADERS = \
  DchDgoPar.h \
  PHAddressObject.h \
  PHReconstructionObject.h \
  PHGeometryObject.h \
  PHCalibrationObject.h \
  PHDchAddressObject.h \
  PHDchGeometryObject.h \
  PHDchCalibrationObject.h \
  PHDchReconstructionObject.h \
  DchBasicCalibration.h \
  PHNoiseObject.h \
  PHDchNoiseObject.h \
  DchBasicNoise.h \
  DchBasicReconstruction.h

noinst_HEADERS = \
  dgoLinkDef.h

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libdgo.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

dgoDict.C: \
  PHAddressObject.h \
  PHReconstructionObject.h \
  PHGeometryObject.h \
  PHCalibrationObject.h \
  PHDchAddressObject.h \
  PHDchGeometryObject.h \
  PHDchCalibrationObject.h \
  PHDchReconstructionObject.h \
  DchBasicCalibration.h \
  PHNoiseObject.h \
  PHDchNoiseObject.h \
  DchBasicNoise.h \
  DchBasicReconstruction.h \
  dgoLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
