AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libvtxrealign.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libvtxrealign_la_LIBADD = \
  -lfun4all \
  -lphool \
  -lsvx


libvtxrealign_la_SOURCES = \
  EventFetcher.C \
  EventLocate.C \
  EventLocatev1.C \
  EventLocateReco.C \
  SingleEventFetcher.C \
  VtxPixelRealignReco.C

nodist_libvtxrealign_la_SOURCES = vtxrealign_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libvtxrealign.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

vtxrealign_Dict.C: \
  EventFetcher.h \
  EventLocate.h \
  EventLocatev1.h \
  EventLocateReco.h \
  SingleEventFetcher.h \
  VtxPixelRealignReco.h \
  vtxrealign_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict.*
