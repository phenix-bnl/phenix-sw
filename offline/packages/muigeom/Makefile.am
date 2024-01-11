AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libmuigeom.la

INCLUDES = -I$(includedir) \
  -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmuigeom_la_LIBADD = -ldgo

noinst_HEADERS = \
  mMuiAddDBEntryLinkDef.h \
  mMuiInitLinkDef.h \
  TMuiAlignLinkDef.h \
  TMuiHVMaskLinkDef.h


include_HEADERS = \
  hash_vector.hh \
  MuiCommon.hh \
  MUIGEOM.h \
  MuiGeomClasses.hh \
  MuiMapInit.h \
  mMuiAddDBEntry.hh \
  mMuiInitModule.h \
  TMuiAddressTable.hh \
  TMuiAlign.h \
  TMuiChannelId.hh \
  TMuiGeometry.hh \
  TMuiHVMask.h \
  TMuiHVTable.hh \
  TMuiPanelGeo.hh \
  TMuiReadoutID.hh \
  TMuiTwoPackGeo.hh


libmuigeom_la_SOURCES = \
  MUIGEOM.cxx \
  MuiMapInit.cc \
  mMuiAddDBEntry.cc \
  mMuiAddDBEntryDict.C \
  mMuiInitModule.C \
  mMuiInitDict.C \
  TMuiAddressTable.cc \
  TMuiAlign.cxx \
  TMuiChannelId.cc \
  TMuiGeometry.cc \
  TMuiHVMask.cxx \
  TMuiHVMaskDict.C \
  TMuiHVTable.cc \
  TMuiPanelGeo.cc \
  TMuiReadoutID.cc \
  TMuiTwoPackGeo.cc

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libmuigeom.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries.
%Dict.C: %Module.h
	rootcint -f $*Dict.C -c $(DEFAULT_INCLUDES) $(INCLUDES) $*Module.h

# Rule for generating CINT dictionaries from class headers.
%Dict.C: %.h %LinkDef.h
	rootcint -f $*Dict.C -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

# Rule for generating CINT dictionaries from class headers.
%Dict.C: %.hh %LinkDef.h
	rootcint -f $*Dict.C -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
