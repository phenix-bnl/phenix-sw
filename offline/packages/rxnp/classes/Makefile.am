AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = librxnp_classes.la

#AM_CXXFLAGS = -pthread

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

include_HEADERS = \
  TRxnpFEMChannel.h\
  TRxnpFEM.h\
  TRxnpScintGeom.h\
  TRxnpGeom.h

librxnp_classes_la_LIBADD = -lpdbcalBase

librxnp_classes_la_SOURCES = \
  TRxnpFEMChannel.cxx\
  TRxnpFEM.cxx\
  TRxnpScintGeom.cxx\
  TRxnpGeom.cxx

noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C
testexternals_LDADD = librxnp_classes.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
