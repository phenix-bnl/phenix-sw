AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = librxnp_interface.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

librxnp_interface_la_LIBADD = \
  -lrxnp_base \
  -lmutoo_core
 
noinst_HEADERS = 

INCLUDES = -I$(GSL_MAIN)/include -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  TRxnpRawScint.h \
  TRxnpRawScint_v1.h \
  TRxnpRawScintMap.h \
  TRxnpRawXang.h \
  TRxnpRawXang_v1.h \
  TRxnpRawXangMap.h \
  TRxnpScint.h \
  TRxnpScint_v1.h \
  TRxnpScintMap.h

##############################################
# please add new classes in alphabetical order

librxnp_interface_la_SOURCES = \
  TRxnpRawScint.cxx \
  TRxnpRawScintDict.C \
  TRxnpRawScint_v1.cxx \
  TRxnpRawScint_v1Dict.C \
  TRxnpRawScintMap.cxx \
  TRxnpRawXang.cxx \
  TRxnpRawXangDict.C \
  TRxnpRawXang_v1.cxx \
  TRxnpRawXang_v1Dict.C \
  TRxnpRawXangMap.cxx \
  TRxnpScint.cxx \
  TRxnpScintDict.C \
  TRxnpScint_v1.cxx \
  TRxnpScint_v1Dict.C \
  TRxnpScintMap.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  librxnp_interface.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

### general root dictionary generation (including LinkDef)
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

### root dictionary generation for files with no LinkDef
%Dict.C: %.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
