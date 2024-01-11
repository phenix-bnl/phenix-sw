AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libCNT.la

noinst_HEADERS = \
  PHCentralTrackLinkDef.h \
  CNTnanoLinkDef.h 

include_HEADERS = \
  PHSnglCentralTrackv1.h \
  PHSnglCentralTrackv2.h \
  PHSnglCentralTrackv3.h \
  PHSnglCentralTrackv4.h \
  PHSnglCentralTrackv5.h \
  PHSnglCentralTrackv6.h \
  PHSnglCentralTrackv7.h \
  PHSnglCentralTrackv9.h \
  PHSnglCentralTrackv10.h \
  PHSnglCentralTrackv11.h \
  PHSnglCentralTrackv12.h \
  PHSnglCentralTrackv13.h \
  PHSnglCentralTrackv14.h \
  PHSnglCentralTrackv15.h \
  PHSnglCentralTrackv16.h \
  PHSnglCentralTrackv17.h \
  PHSnglCentralTrackv18.h \
  PHSnglCentralTrackv19.h \
  PHSnglCentralTrackv20.h \
  PHSnglCentralTrackv21.h \
  PHSnglCentralTrackv22.h \
  PHSnglCentralTrackv23.h \
  PHSnglCentralTrackv24.h \
  PHCentralTrackv1.h \
  PHCentralTrackv2.h \
  PHCentralTrackv3.h \
  PHCentralTrackv4.h \
  PHCentralTrackv5.h \
  PHCentralTrackv6.h \
  PHCentralTrackv7.h \
  PHCentralTrackv9.h \
  PHCentralTrackv10.h \
  PHCentralTrackv11.h \
  PHCentralTrackv12.h \
  PHCentralTrackv13.h \
  PHCentralTrackv14.h \
  PHCentralTrackv15.h \
  PHCentralTrackv16.h \
  PHCentralTrackv17.h \
  PHCentralTrackv18.h \
  PHCentralTrackv19.h \
  PHCentralTrackv20.h \
  PHCentralTrackv21.h \
  PHCentralTrackv22.h \
  PHCentralTrackv23.h \
  PHCentralTrackv24.h \
  ACCInclusiveNanoCutsv1.h \
  CNTInclusiveNanoCutsv1.h \
  CNTInclusiveNanoCutsv2.h \
  CNTnanoCuts.h \
  V24maker.h \
  PCTracking.h

libCNT_la_SOURCES = \
 ACCInclusiveNanoCutsv1.C \
 CNTInclusiveNanoCutsv1.C \
 CNTInclusiveNanoCutsv2.C \
 CNTnanoCuts.C \
 V24maker.C \
 PHCentralTrackv1.C \
 PHCentralTrackv2.C \
 PHCentralTrackv3.C \
 PHCentralTrackv4.C \
 PHCentralTrackv5.C \
 PHCentralTrackv6.C \
 PHCentralTrackv7.C \
 PHCentralTrackv9.C \
 PHCentralTrackv10.C \
 PHCentralTrackv11.C \
 PHCentralTrackv12.C \
 PHCentralTrackv13.C \
 PHCentralTrackv14.C \
 PHCentralTrackv15.C \
 PHCentralTrackv16.C \
 PHCentralTrackv17.C \
 PHCentralTrackv18.C \
 PHCentralTrackv19.C \
 PHCentralTrackv20.C \
 PHCentralTrackv21.C \
 PHCentralTrackv22.C \
 PHCentralTrackv23.C \
 PHCentralTrackv24.C \
 PHSnglCentralTrackv1.C \
 PHSnglCentralTrackv2.C \
 PHSnglCentralTrackv3.C \
 PHSnglCentralTrackv4.C \
 PHSnglCentralTrackv5.C \
 PHSnglCentralTrackv6.C \
 PHSnglCentralTrackv7.C \
 PHSnglCentralTrackv9.C \
 PHSnglCentralTrackv10.C \
 PHSnglCentralTrackv11.C \
 PHSnglCentralTrackv12.C \
 PHSnglCentralTrackv13.C \
 PHSnglCentralTrackv14.C \
 PHSnglCentralTrackv15.C \
 PHSnglCentralTrackv16.C \
 PHSnglCentralTrackv17.C \
 PHSnglCentralTrackv18.C \
 PHSnglCentralTrackv19.C \
 PHSnglCentralTrackv20.C \
 PHSnglCentralTrackv21.C \
 PHSnglCentralTrackv22.C \
 PHSnglCentralTrackv23.C \
 PHSnglCentralTrackv24.C \
 PCTracking.C
nodist_libCNT_la_SOURCES = \
 CNTnano_Dict.C \
 PHCentralTrack_Dict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libCNT_la_LIBADD = \
  -lcrk \
  -lCrkPID \
  -lemc \
  -lnanoDST \
  -lpad \
  -lphgeo \
  -lphool

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libCNT.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.

PHCentralTrack_Dict.C: \
  PHCentralTrackv1.h  PHSnglCentralTrackv1.h \
  PHCentralTrackv2.h  PHSnglCentralTrackv2.h \
  PHCentralTrackv3.h  PHSnglCentralTrackv3.h \
  PHCentralTrackv4.h  PHSnglCentralTrackv4.h \
  PHCentralTrackv5.h  PHSnglCentralTrackv5.h \
  PHCentralTrackv6.h  PHSnglCentralTrackv6.h \
  PHCentralTrackv7.h  PHSnglCentralTrackv7.h \
  PHCentralTrackv9.h  PHSnglCentralTrackv9.h \
  PHCentralTrackv10.h PHSnglCentralTrackv10.h \
  PHCentralTrackv11.h PHSnglCentralTrackv11.h \
  PHCentralTrackv12.h PHSnglCentralTrackv12.h \
  PHCentralTrackv13.h PHSnglCentralTrackv13.h \
  PHCentralTrackv14.h PHSnglCentralTrackv14.h \
  PHCentralTrackv15.h PHSnglCentralTrackv15.h \
  PHCentralTrackv16.h PHSnglCentralTrackv16.h \
  PHCentralTrackv17.h PHSnglCentralTrackv17.h \
  PHCentralTrackv18.h PHSnglCentralTrackv18.h \
  PHCentralTrackv18.h PHSnglCentralTrackv18.h \
  PHCentralTrackv19.h PHSnglCentralTrackv19.h \
  PHCentralTrackv20.h PHSnglCentralTrackv20.h \
  PHCentralTrackv21.h PHSnglCentralTrackv21.h \
  PHCentralTrackv22.h PHSnglCentralTrackv22.h \
  PHCentralTrackv23.h PHSnglCentralTrackv23.h \
  PHCentralTrackv24.h PHSnglCentralTrackv24.h \
  PHCentralTrackLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

CNTnano_Dict.C: \
  CNTnanoCuts.h \
  ACCInclusiveNanoCutsv1.h \
  CNTInclusiveNanoCutsv1.h \
  CNTInclusiveNanoCutsv2.h \
  V24maker.h \
  CNTnanoLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*


testexternals_SOURCES = testexternals.C
