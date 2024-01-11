AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libEmcStatic.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libEmcStatic_la_LIBADD = -lemcDataManager -lphgeo

libEmcStatic_la_SOURCES = \
  emcException.C \
  SecGeom.C \
  emcGeometry.C \
  EmcIndexer.C \
  EmcSector.C \
  EmcStaticData.C \
  EmcSuperModule.C \
  PbGlCalibrationData.C \
  PbGlIndexer.C \
  PbGlSector.C \
  PbGlSuperModule.C \
  PbScCalibrationData.C \
  PbScIndexer.C \
  PbScSector.C \
  PbScSuperModule.C \
  StaticCint.C \
  emcCalibrationData.C \
  emcGlobals.C

include_HEADERS = \
  emcDefines.h \
  emcException.h \
  SecGeom.h \
  emcGeometry.h \
  EmcIndexer.h \
  EmcSector.h \
  EmcStaticData.h \
  EmcSuperModule.h \
  PbGlCalibrationData.h \
  PbGlIndexer.h \
  PbGlSector.h \
  PbGlSuperModule.h \
  PbScCalibrationData.h \
  PbScIndexer.h \
  PbScSector.h \
  PbScSuperModule.h \
  emcCalibrationData.h \
  emcGlobals.h

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libEmcStatic.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


StaticCint.C : \
  EmcIndexer.h EmcSector.h EmcStaticData.h EmcSuperModule.h \
  PbGlIndexer.h PbGlSector.h PbGlCalibrationData.h PbGlSuperModule.h \
  PbScIndexer.h PbScSector.h PbScCalibrationData.h PbScSuperModule.h \
  emcGlobals.h emcCalibrationData.h SecGeom.h emcGeometry.h StaticLinkDef.h
	rootcint -f $@ -c $(DEFS) $(INCLUDES) $^

clean-local:
	rm -f *Cint*


testexternals_SOURCES = testexternals.C
