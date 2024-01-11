AUTOMAKE_OPTIONS = foreign

######################################
# this is the generic include path to be used
AM_CPPFLAGS = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

lib_LTLIBRARIES = libfvtxoo_alignment.la

libfvtxoo_alignment_la_LIBADD = \
  -lfvtxgeom \
  -lmuigeom \
  -lmuioo_base \
  -lmutgeom \
  -lfvtxoo_base \
  -lmutoo_alignment

##############################################
# please add new classes in alphabetical order

noinst_HEADERS = \
  TFvtxAlignParLinkDef.h \
  TFvtxAlignPar_v1LinkDef.h \
  TFvtxAlignmentCorrectionLinkDef.h \
  TFvtxMILLEPEDELinkDef.h

include_HEADERS = \
  TFvtxAlignmentCorrection.h \
  TFvtxAlignPar.hh \
  TFvtxAlignPar_v1.hh \
  TFvtxAlignParMap.h \
  TFvtxMILLEPEDE.h \
  TFvtxMPNode.h \
  TFvtxMPTrack.h \
  TForwardMPTrack.h

##############################################
# please add new classes in alphabetical order

libfvtxoo_alignment_la_SOURCES = \
  TFvtxAlignmentCorrection.cxx \
  TFvtxAlignmentCorrectionDict.C \
  TFvtxAlignPar.cxx \
  TFvtxAlignParDict.C \
  TFvtxAlignPar_v1.cxx \
  TFvtxAlignPar_v1Dict.C \
  TFvtxAlignParMap.cxx \
  TFvtxMILLEPEDE.cxx \
  TFvtxMPNode.cxx \
  TFvtxMPTrack.cxx \
  TForwardMPTrack.cxx \
  TFvtxMILLEPEDE_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C

testexternals_LDADD = \
  libfvtxoo_alignment.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# root dictionary
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

%Dict.C: %.hh %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

TFvtxMILLEPEDE_Dict.C: \
  TFvtxMILLEPEDE.h \
  TFvtxMPNode.h \
  TFvtxMPTrack.h \
  TForwardMPTrack.h \
  TFvtxMILLEPEDELinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

