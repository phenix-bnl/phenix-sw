# $Id: Makefile.am,v 1.23 2008/08/02 15:47:50 hpereira Exp $
AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libmutoo_display.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmutoo_display_la_LIBADD = \
  -lmutoo_interface \
  -lmuioo_interface
 
##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  PhMutooDisplay.h \
  PHObj3D.h \
  TMutAnode3D.h \
  TMutCoord3D.h \
  TMutGap3D.h \
  TMutIndex.h \
  TMutMCTrack3D.h \
  TMutOctantView.h \
  TMutPlaneView.h \
  TMutSideView.h \
  TMutHalfOctantBidimView.h \
  TMutStub3D.h \
  TMutTrack3D.h \
  TMutWire3D.h

##############################################
# please add new classes in alphabetical order

libmutoo_display_la_SOURCES =  \
  PhMutooDisplay.cxx  \
  PHObj3D.cxx \
  TMutAnode3D.cxx \
  TMutCoord3D.cxx \
  TMutGap3D.cxx \
  TMutIndex.cxx \
  TMutMCTrack3D.cxx \
  TMutOctantView.cxx \
  TMutPlaneView.cxx \
  TMutSideView.cxx \
  TMutHalfOctantBidimView.cxx \
  TMutStub3D.cxx \
  TMutTrack3D.cxx \
  TMutWire3D.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals
  
testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmutoo_display.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f $(BUILT_SOURCES)
