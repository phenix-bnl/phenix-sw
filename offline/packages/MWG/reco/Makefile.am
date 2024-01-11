AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libMWGOO.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libMWGOO_la_LIBADD = \
  -lpdbcalBase \
  -lMWG_interface \
  -lMWG_tools \
  -lmuioo_interface \
  -lmutgeom \
  -lmutoo_display \
  -lfvtxoo_modules  \
  -lfvtxoo_alignment  \
  -lmutoo_modules

include_HEADERS = \
  FvtxConeTracklets.h 

noinst_HEADERS = \
  MWGOOReco.h \
  MWGOORecoLinkDef.h \
  MWGRecal.h \
  MWGRecalLinkDef.h \
  MWGReco.h \
  MWGRecoLinkDef.h \
  PHMuoTrackAdcReco.h \
  PHMuoTrackAdcRecoLinkDef.h \
  MuoAdcToMutooHit.h \
  MuoAdcToMutooHitLinkDef.h \
  MWGFvtxReco.h \
  MWGFvtxRecoLinkDef.h \
  FvtxConeTrackletsLinkDef.h

libMWGOO_la_SOURCES = \
  MuonNdstAna.C \
  MuonNdstAna_Dict.C \
  MutEffic.C \
  MutEffic_Dict.C \
  MWGRecal.C \
  MWGRecal_Dict.C \
  MWGOOReco.C \
  MWGOOReco_Dict.C \
  PHMuoTrackAdcReco.C \
  PHMuoTrackAdcReco_Dict.C \
  MuoAdcToMutooHit.C \
  MuoAdcToMutooHit_Dict.C \
  MWGFvtxReco.C \
  MWGFvtxReco_Dict.C \
  FvtxConeTracklets.cxx \
  FvtxConeTracklets_Dict.C

### general root dictionary generation (including LinkDef)
%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

# executables needed to make sure libraries are properly linked
noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C
testexternals_LDADD =  libMWGOO.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
