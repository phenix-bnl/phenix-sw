# $Id: Makefile.am,v 1.4 2008/08/02 15:48:04 hpereira Exp $
AUTOMAKE_OPTIONS = foreign

AM_CXXFLAGS = 

lib_LTLIBRARIES = librxnp_modules.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

librxnp_modules_la_LIBADD = \
 -lrxnp_interface \
 -lrxnp_classes \
 -lbbc
 
noinst_HEADERS = 

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  mRxnpCalXang.h \
  mRxnpCalXangPar.h \
  mRxnpUnpackPRDF.h \
  mRxnpUnpackPRDFPar.h \
  TRxnpParBase.h

##############################################
# please add new classes in alphabetical order

librxnp_modules_la_SOURCES = \
  mRxnpCalXang.cxx \
  mRxnpUnpackPRDF.cxx

noinst_PROGRAMS = testexternals  
testexternals_SOURCES = testexternals.C
testexternals_LDADD = librxnp_modules.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
