# $Id: Makefile.am,v 1.11 2014/01/26 18:01:07 bbannier Exp $
AUTOMAKE_OPTIONS = foreign

AM_CXXFLAGS = -Wall -Werror

lib_LTLIBRARIES = librpcoo_modules.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

librpcoo_modules_la_LIBADD = \
 -lrpcoo_interface \
 -lmutoo_interface 
 
noinst_HEADERS = 

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  mRpcEmbed.h \
  mRpcEmbedPar.h \
  mRpcFindClus.h \
  mRpcFindClusPar.h \
  mRpcFitClus.h \
  mRpcFitClusPar.h \
  mRpcHodoUnpack.h \
  mRpcPackPRDF.h \
  mRpcResponse.h \
  mRpcResponsePar.h \
  mRpcRoad.h \
  mRpcSlowSim.h \
  mRpcSlowSimPar.h \
  mRpcTrack.h \
  mRpcUnpack.h \
  TRpcParBase.h

##############################################
# please add new classes in alphabetical order

librpcoo_modules_la_SOURCES = \
  mRpcEmbed.cxx \
  mRpcFindClus.cxx \
  mRpcFitClus.cxx \
  mRpcHodoUnpack.cxx \
  mRpcPackPRDF.cxx \
  mRpcResponse.cxx \
  mRpcRoad.cxx \
  mRpcSlowSim.cxx \
  mRpcTrack.cxx \
  mRpcUnpack.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals
  
testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  librpcoo_modules.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
