AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libMWG_interface.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libMWG_interface_la_LIBADD = \
  -lboost_regex \
  -lnanoDST \
  -lmutoo_core

include_HEADERS = \
  MWG.h \
  MWGVersion.h \
  MWGVertex.h \
  PHMuoTrackv8.h \
  PHMuoTracksv8.h \
  PHMuoTrackAdc.h \
  PHMuoTracksAdc.h \
  rcp.h

noinst_HEADERS = \
  MWGInclusiveNanoCutsv1.h \
  MWGInclusiveNanoCutsv2.h \
  MWGInclusiveNanoCutsLinkDef.h \
  PHMuoTrackPrivate.h \
  PHMuoTrackv1.h \
  PHMuoTrackv2.h \
  PHMuoTrackv3.h \
  PHMuoTrackv4.h \
  PHMuoTrackv5.h \
  PHMuoTrackv6.h \
  PHMuoTrackv7.h \
  PHMuoTrackv9.h \
  PHMuoTrackv10.h \
  PHMuoTrackv11.h \
  PHMuoTrackv12.h \
  PHMuoTrackv13.h \
  PHMuoTrackv14.h \
  PHMuoTrackv15.h \
  PHMuoTrackv16.h \
  PHdiMuoTrackv1.h \
  PHdiMuoTrackv2.h \
  PHdiMuoTrackv3.h \
  PHdiMuoTrackv4.h \
  PHdiMuoTrackv5.h \
  PHMuoTrackAdcv1.h \
  PHMuoTracksv1.h \
  PHMuoTracksv2.h \
  PHMuoTracksv3.h \
  PHMuoTracksv4.h \
  PHMuoTracksv5.h \
  PHMuoTracksv6.h \
  PHMuoTracksv7.h \
  PHMuoTracksv9.h \
  PHMuoTracksv10.h \
  PHMuoTracksv11.h \
  PHMuoTracksv12.h \
  PHMuoTracksv13.h \
  PHMuoTracksv14.h \
  PHMuoTracksv15.h \
  PHMuoTracksv16.h \
  PHdiMuoTracksv1.h \
  PHdiMuoTracksv2.h \
  PHdiMuoTracksv3.h \
  PHdiMuoTracksv4.h \
  PHdiMuoTracksv5.h \
  PHdiMuoTracksv6.h \
  PHdiMuoTracksv7.h \
  PHdiMuoTracksv8.h \
  PHdiMuoTracksv9.h \
  PHdiMuoTracksv10.h \
  PHdiMuoTracksv11.h \
  PHdiMuoTracksv12.h \
  PHdiMuoTracksv13.h \
  PHdiMuoTracksv14.h \
  PHdiMuoTracksv15.h \
  PHdiMuoTracksv16.h \
  PHMuoTracksAdcv1.h \
  PHMuoTracksLinkDef.h

libMWG_interface_la_SOURCES = \
  MWG.C \
  MWGInclusiveNanoCuts_Dict.C \
  MWGInclusiveNanoCutsv1.C \
  MWGInclusiveNanoCutsv2.C \
  MWGVersion.C \
  MWGVertex.C \
  PHMuoTracks_Dict.C \
  PHMuoTrackPrivate.C \
  PHMuoTrackv1.C \
  PHMuoTrackv2.C \
  PHMuoTrackv3.C \
  PHMuoTrackv4.C \
  PHMuoTrackv5.C \
  PHMuoTrackv6.C \
  PHMuoTrackv7.C \
  PHMuoTrackv8.C \
  PHMuoTrackv9.C \
  PHMuoTrackv10.C \
  PHMuoTrackv11.C \
  PHMuoTrackv12.C \
  PHMuoTrackv13.C \
  PHMuoTrackv14.C \
  PHMuoTrackv15.C \
  PHMuoTrackv16.C \
  PHdiMuoTrackv1.C \
  PHdiMuoTrackv2.C \
  PHdiMuoTrackv3.C \
  PHdiMuoTrackv4.C \
  PHdiMuoTrackv5.C \
  PHMuoTrackAdc_Dict.C \
  PHMuoTrackAdc.C \
  PHMuoTrackAdcv1.C \
  PHMuoTracksv1.C \
  PHMuoTracksv2.C \
  PHMuoTracksv3.C \
  PHMuoTracksv4.C \
  PHMuoTracksv5.C \
  PHMuoTracksv6.C \
  PHMuoTracksv7.C \
  PHMuoTracksv8.C \
  PHMuoTracksv9.C \
  PHMuoTracksv10.C \
  PHMuoTracksv11.C \
  PHMuoTracksv12.C \
  PHMuoTracksv13.C \
  PHMuoTracksv14.C \
  PHMuoTracksv15.C \
  PHMuoTracksv16.C \
  PHdiMuoTracksv1.C \
  PHdiMuoTracksv2.C \
  PHdiMuoTracksv3.C \
  PHdiMuoTracksv4.C \
  PHdiMuoTracksv5.C \
  PHdiMuoTracksv6.C \
  PHdiMuoTracksv7.C \
  PHdiMuoTracksv8.C \
  PHdiMuoTracksv9.C \
  PHdiMuoTracksv10.C \
  PHdiMuoTracksv11.C \
  PHdiMuoTracksv12.C \
  PHdiMuoTracksv13.C \
  PHdiMuoTracksv14.C \
  PHdiMuoTracksv15.C \
  PHdiMuoTracksv16.C \
  PHMuoTracksAdc.C \
  PHMuoTracksAdcv1.C \
  rcp.C

### general root dictionary generation (including LinkDef)
%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

# Rule for generating CINT dictionaries from class headers.
MWGInclusiveNanoCuts_Dict.C: \
  MWGInclusiveNanoCutsv1.h \
  MWGInclusiveNanoCutsv2.h \
  MWGInclusiveNanoCutsLinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHMuoTracks_Dict.C: \
  PHMuoTrackv1.h \
  PHMuoTrackv2.h \
  PHMuoTrackv3.h \
  PHMuoTrackv4.h \
  PHMuoTrackv5.h \
  PHMuoTrackv6.h \
  PHMuoTrackv7.h \
  PHMuoTrackv8.h \
  PHMuoTrackv9.h \
  PHMuoTrackv10.h \
  PHMuoTrackv11.h \
  PHMuoTrackv12.h \
  PHMuoTrackv13.h \
  PHMuoTrackv14.h \
  PHMuoTrackv15.h \
  PHMuoTrackv16.h \
  PHMuoTracksv1.h \
  PHMuoTracksv2.h \
  PHMuoTracksv3.h \
  PHMuoTracksv4.h \
  PHMuoTracksv5.h \
  PHMuoTracksv6.h \
  PHMuoTracksv7.h \
  PHMuoTracksv8.h \
  PHMuoTracksv9.h \
  PHMuoTracksv10.h \
  PHMuoTracksv11.h \
  PHMuoTracksv12.h \
  PHMuoTracksv13.h \
  PHMuoTracksv14.h \
  PHMuoTracksv15.h \
  PHMuoTracksv16.h \
  PHdiMuoTrackv1.h \
  PHdiMuoTrackv2.h \
  PHdiMuoTrackv3.h \
  PHdiMuoTrackv4.h \
  PHdiMuoTrackv5.h \
  PHdiMuoTrackv5_Container.h \
  PHdiMuoTracksv1.h \
  PHdiMuoTracksv2.h \
  PHdiMuoTracksv3.h \
  PHdiMuoTracksv4.h \
  PHdiMuoTracksv5.h \
  PHdiMuoTracksv6.h \
  PHdiMuoTracksv7.h \
  PHdiMuoTracksv8.h \
  PHdiMuoTracksv9.h \
  PHdiMuoTracksv10.h \
  PHdiMuoTracksv11.h \
  PHdiMuoTracksv12.h \
  PHdiMuoTracksv13.h \
  PHdiMuoTracksv14.h \
  PHdiMuoTracksv15.h \
  PHdiMuoTracksv16.h \
  PHMuoTracksLinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHMuoTrackAdc_Dict.C: \
  PHMuoTrackAdc.h \
  PHMuoTrackAdcv1.h \
  PHMuoTracksAdc.h \
  PHMuoTracksAdcv1.h \
  PHMuoTrackAdcLinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

# executables needed to make sure libraries are properly linked
noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = libMWG_interface.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
