AUTOMAKE_OPTIONS = foreign

AM_FFLAGS = -fno-second-underscore -Wall -ffixed-line-length-none
AM_CXXFLAGS = -Wall -Werror

lib_LTLIBRARIES = libmfm_base.la libmfm.la

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(CERN)/$(CERN_LEVEL)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmfm_base_la_LDFLAGS = \
  ${AM_LDFLAGS} \
  `root-config --libs` \
  `$(OPT_PHENIX)/bin/phenix_cernlib.pl`

libmfm_base_la_LIBADD = \
  $(FLIBS)

libmfm_la_LIBADD = libmfm_base.la

include_HEADERS = \
  gcmate.inc \
  mfm.inc \
  mMfmMT.h \
  PHBFieldMap.h

## removed the following two magfield maps
## cause they are old, unused, and allocate
## useless memory.
#  bfield3d99.F
#  bfield3d01.F


libmfm_base_la_SOURCES = \
  bfield.F \
  czechsteel.F \
  field3dmap.F \
  field3dmap01.F \
  fugawe.F \
  gufldold.F \
  g77trigdef.F \
  hunt.F \
  isitsteel.F \
  polint.F \
  bfield3d03.cc \
  read3droot.cc \
  read3droot01.cc \
  PHBFieldMap.cc \
  PHBFieldMapDict.C

libmfm_la_SOURCES = \
  gufld.F \
  mfm_initialize.F \
  mfm_getfscale.F \
  mfm_setfscale.F \
  mMfmMTDict.C \
  mMfmMT.cc

noinst_PROGRAMS = \
  testexternals_base \
  testexternals

BUILT_SOURCES = testexternals.C

testexternals_base_SOURCES = \
  testexternals.C

testexternals_base_LDADD = \
  libmfm_base.la

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmfm.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
