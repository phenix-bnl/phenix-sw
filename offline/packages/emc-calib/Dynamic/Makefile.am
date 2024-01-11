AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = libEmcDynamic.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libEmcDynamic_la_LIBADD = \
  -lEmcStatic \
  -lEvent \
  -lmessage

libEmcDynamic_la_SOURCES = \
  DynamicCint.C \
  EmcDynamicData.C \
  EmcFEM.C \
  emcDataFormatter.C \
  emcConfigurationFile.C \
  emcDataObject.C \
  emcRawDataAccessor.C \
  emcRawDataObject.C

include_HEADERS = \
  EmcDynamicData.h \
  EmcFEM.h \
  emcDataFormatter.h \
  emcConfigurationFile.h \
  emcDataObject.h \
  emcRawDataAccessor.h \
  emcRawDataObject.h

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libEmcDynamic.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

DynamicCint.C : \
  EmcFEM.h  emcRawDataAccessor.h emcDataObject.h \
  emcRawDataObject.h emcConfigurationFile.h \
  EmcDynamicData.h emcDataFormatter.h DynamicLinkDef.h
	rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Cint*





testexternals_SOURCES = testexternals.C
