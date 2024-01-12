
AUTOMAKE_OPTIONS = foreign

ROOTLIBS := `root-config --libs`
ROOTCFLAGS := `root-config --cflags`

# The flags -fno-automatic -finit-local-zero are taken from
# event_gen/lib/flags*mak -ffloat-store and/or -mfpmath=sse were
# suggested by Yue Shi Lai to address a possible g77 compiler bug
# where the RHS of a logical comparison is not precalculated.
# We use -msee instead of -msse2 in case there are PIII's floating
# around.
# DLW: -Wtabs and -Wno-unused-label (note the 's' was dropped in 4.3.2) are used to suppress warnings I feel are not issues,
# and may serve only to hide ones that you need to worry about. (ie. the .f code has a lot of these!)
AM_FFLAGS =  -mfpmath=sse -ffixed-line-length-none -fsecond-underscore -Wall -fno-automatic -msse -Wtabs -Wno-unused-label

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib $(ROOTLIBS)

AM_CPPFLAGS = -I${includedir} -I${OFFLINE_MAIN}/include ${ROOTCFLAGS}

noinst_HEADERS = \
 ConfigParser.h \
 event.inc \
 hijingpar.inc \
 hijingpar.h \
 himain.inc \
 histrng.h

include_HEADERS = \
 PHHijing.h \
 PHHijingHeader.h \
 PHHijingHeaderV1.h \
 PHHijingHeaderV2.h \
 PHHijingHeaderV3.h \
 PHHijingHeaderV4.h \
 PHHijingOscarOutputManager.h \
 THijing.h

lib_LTLIBRARIES = libPHHijing.la

libPHHijing_la_SOURCES = \
 hijing_init.f \
 hijing1_37.f \
 hijingpisa.f \
 hijingranf.f \
 hijingseed.f \
 hipyset1_35.f \
 PHHijing.C \
 PHHijingDict.C \
 PHHijingHeader.C \
 PHHijingHeaderDict.C \
 PHHijingHeaderV1.C \
 PHHijingHeaderV1Dict.C \
 PHHijingHeaderV2.C \
 PHHijingHeaderV2Dict.C \
 PHHijingHeaderV3.C \
 PHHijingHeaderV3Dict.C \
 PHHijingHeaderV4.C \
 PHHijingHeaderV4Dict.C \
 PHHijingOscarOutputManager.C \
 PHHijingOscarOutputManagerDict.C \
 THijing.C \
 THijingDict.C

libPHHijing_la_LDFLAGS = \
  ${AM_LDFLAGS} \
  `$(OPT_PHENIX)/bin/phenix_cernlib.pl`

libPHHijing_la_LIBADD = \
 -lPHPythia \
  $(FLIBS) \
 -lPHHepMCNode \
 -lHepMC

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libPHHijing.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c ${AM_CPPFLAGS} $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict* ${BUILT_SOURCES}
