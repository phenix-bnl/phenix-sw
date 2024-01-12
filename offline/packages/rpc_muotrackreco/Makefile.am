AUTOMAKE_OPTIONS = foreign

######################################
# this is the generic include path to be used
INCLUDES = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  librpc_muotrackreco.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib 

librpc_muotrackreco_la_LIBADD = \
  -lfun4all \
  -lSubsysReco_muons \
  -lMWGpico \
  -lrpcgeom \
  -lrpcoo_interface \
  -lrpcoo_modules

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  MWGRpcMuoTrackReco.h \
  MWGRpcMuoTrackRecoLinkDef.h \
  RpcMuoReco.h \
  RpcMuoRecoLinkDef.h


##############################################
# please add new classes in alphabetical order

librpc_muotrackreco_la_SOURCES = \
  MWGRpcMuoTrackReco.C \
  MWGRpcMuoTrackReco_Dict.C \
  RpcMuoReco.cxx \
  RpcMuoReco_Dict.C


# root dictionaries
%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  librpc_muotrackreco.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
