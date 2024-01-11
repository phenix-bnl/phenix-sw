
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmuioo_base.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmuioo_base_la_LIBADD =  \
  -lmuigeom

include_HEADERS = \
  MUIOO.h \
  MUIOO_HASH_MAP.h \
  TMuiGeo.h \
  TMuiEvalRes.hh \
  TMuiKeyGen.h \
  TMuiMCTwoPack.h

libmuioo_base_la_SOURCES = \
  MUIOO.cxx \
  TMuiEvalRes.cxx \
  TMuiEvalResDict.C \
  TMuiGeo.cxx \
  TMuiGeoDict.C \
  TMuiKeyGen.cxx \
  TMuiMCTwoPack.cxx \
  TMuiMCTwoPackDict.C

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C 

testexternals_LDADD = \
  libmuioo_base.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# rules to generate root dictionaries
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.hh %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
