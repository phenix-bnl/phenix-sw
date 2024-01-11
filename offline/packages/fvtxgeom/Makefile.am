
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libfvtxgeom.la

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include 

libfvtxgeom_la_LDFLAGS = \
  `root-config --evelibs`

libfvtxgeom_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -L$(OPT_PHENIX)/lib \
  -lphgeo \
  -lpdbcalBase \
  -lfun4all \
  -lodbc++

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  FVTXGEOM.h \
  FvtxArm.h \
  FvtxCage.h \
  FvtxColumn.h \
  FvtxDCMChannelMap.h \
  FvtxGeom.h \
  TFvtxDatabaseCntrl.h \
  TFvtxGlobalParCntrl.h \
  TFvtxDeadMap.h \
  FvtxGeomObject.h \
  FvtxSector.h \
  FvtxStation.h \
  FvtxStrip.h \
  TFvtxAlign.h \
  TFvtxIndex.h

##############################################
# please add new classes in alphabetical order

libfvtxgeom_la_SOURCES = \
  FVTXGEOM.cxx \
  FVTXGEOMDict.C \
  FvtxArmDict.C \
  FvtxCage.cxx \
  FvtxCageDict.C \
  FvtxColumn.cxx  \
  FvtxColumnDict.C \
  FvtxDCMChannelMap.cxx \
  FvtxDCMChannelMapDict.C \
  FvtxGeom.cxx  \
  FvtxGeomDict.C  \
  TFvtxDatabaseCntrl.cxx  \
  TFvtxDatabaseCntrlDict.C  \
  TFvtxGlobalParCntrl.cxx \
  TFvtxGlobalParCntrlDict.C  \
  TFvtxDeadMap.cxx  \
  TFvtxDeadMapDict.C  \
  FvtxGeomObjectDict.C \
  FvtxGeomObject.cxx \
  FvtxSector.cxx \
  FvtxSectorDict.C \
  FvtxStation.cxx \
  FvtxStationDict.C \
  FvtxStrip.cxx \
  FvtxStripDict.C \
  TFvtxAlign.cxx \
  TFvtxIndex.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libfvtxgeom.la

testexternals_SOURCES = \
  testexternals.C

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


# rules for root dictionaries
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
