AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  librxnp_subsysreco.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

librxnp_subsysreco_la_LIBADD = \
  -lSubsysReco_muons \
  -lrxnp_modules \
  -ltrigger \
  -lzdc

##############################################
# please add new classes in alphabetical order
include_HEADERS = \
  RxnpAnaDst.h \
  RxnpReco.h

##############################################
# please add new classes in alphabetical order

librxnp_subsysreco_la_SOURCES = \
  RxnpAnaDst.cxx \
  RxnpAnaDstDict.C \
  RxnpReco.cxx \
  RxnpRecoDict.C

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  librxnp_subsysreco.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# rules to make root dictionaries
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
