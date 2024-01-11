AUTOMAKE_OPTIONS = foreign

######################################
# this is the generic include path to be used
AM_CPPFLAGS = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

lib_LTLIBRARIES = libmutoo_alignment.la

libmutoo_alignment_la_LIBADD = -lmutoo_base @FLIBS@

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  MILLEPEDE.h \
  TMutAlignmentCorrection.h \
  TMutAlignPar.hh \
  TMutAlignPar_v1.hh \
  TMutAlignParMap.h

##############################################
# please add new classes in alphabetical order

libmutoo_alignment_la_SOURCES = \
  millepede.f \
  TMutAlignmentCorrection.cxx \
  TMutAlignmentCorrectionDict.C \
  TMutAlignPar.cxx \
  TMutAlignParDict.C \
  TMutAlignPar_v1.cxx \
  TMutAlignPar_v1Dict.C \
  TMutAlignParMap.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libmutoo_alignment.la

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

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
