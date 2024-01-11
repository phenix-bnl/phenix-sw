AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = librxnp_base.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

librxnp_base_la_LIBADD = \
  -lmutoo_core
 
include_HEADERS = \
  RXNP.h \
  TRxnpKeyGen.h

librxnp_base_la_SOURCES = \
  TRxnpKeyGen.cxx 

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  librxnp_base.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
