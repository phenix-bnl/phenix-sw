
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libfvtxoo_display.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libfvtxoo_display_la_LIBADD = \
 -lmutoo_display \
 -lfvtxgeom \
 -lfvtxoo_base \
 -lfvtxoo_interface 

include_HEADERS = \
  TFvtxColumn3D.h \
  TFvtxMCHit3D.h \
  TFvtxMCTrack3D.h \
  TFvtxSector3D.h \
  TFvtxTrack3D.h

libfvtxoo_display_la_SOURCES = \
  TFvtxColumn3D.cxx \
  TFvtxMCHit3D.cxx \
  TFvtxMCTrack3D.cxx \
  TFvtxSector3D.cxx \
  TFvtxTrack3D.cxx

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libfvtxoo_display.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
