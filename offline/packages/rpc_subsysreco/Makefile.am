AUTOMAKE_OPTIONS = foreign

######################################
# this is the generic include path to be used
INCLUDES = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  librpc_subsysreco.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib 

librpc_subsysreco_la_LIBADD = \
  -lfun4all \
  -lSubsysReco_muons \
  -lrpcoo_interface \
  -lrpcoo_modules

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  RpcHodoUnpackPRDF.h \
  RpcReadbackDST.h \
  RpcReco.h \
  RpcUnpackPisa.h \
  RpcUnpackPRDF.h \
  RpcUnpackSim.h \
  RpcAnaTuples.h

##############################################
# please add new classes in alphabetical order

librpc_subsysreco_la_SOURCES = \
  RpcHodoUnpackPRDF.cxx \
  RpcHodoUnpackPRDFDict.C \
  RpcReadbackDST.cxx \
  RpcReadbackDSTDict.C \
  RpcReco.cxx \
  RpcRecoDict.C \
  RpcUnpackPisa.cxx \
  RpcUnpackPisaDict.C \
  RpcUnpackPRDF.cxx \
  RpcUnpackPRDFDict.C \
  RpcUnpackSim.cxx \
  RpcUnpackSimDict.C \
  RpcAnaTuples.cxx \
  RpcAnaTuplesDict.C 

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  librpc_subsysreco.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# root dictionaries
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
