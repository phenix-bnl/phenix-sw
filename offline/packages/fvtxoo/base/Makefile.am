
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libfvtxoo_base.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =-I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libfvtxoo_base_la_LIBADD = -lmutoo_core

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  FVTXOO.h \
  FVTXOO_FEM.h \
  TFvtxActivePacketSet.h \
  TFvtxKeyGen.h 

##############################################
# please add new classes in alphabetical order

libfvtxoo_base_la_SOURCES = \
  FVTXOO.cxx \
  FVTXOODict.C \
  TFvtxKeyGen.cxx
  
noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libfvtxoo_base.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


# rules for root dictionaries
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
