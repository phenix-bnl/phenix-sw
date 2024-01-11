AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libmuon_subsysreco.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

# libmuon_subsysreco_la_LIBADD = \
#   -lmuon_util \
#   -lmutoo_modules \
#   -lmuioo_modules \
#   -lmuioo_display \
#   -lrpcoo_display \
#   -lrpcoo_interface \
#   -lfvtxoo_display \
#   -lfvtxoo_interface \
#   -lrpcoo_display 

### temporarely disable rpc
libmuon_subsysreco_la_LIBADD = \
  -lmuon_util \
  -lmutoo_modules \
  -lmuioo_modules \
  -lmuioo_display \
  -lfvtxoo_display \
  -lfvtxoo_interface \
  -lrpcoo_display 

##############################################
# please add new classes in alphabetical order

noinst_HEADERS = \
  Muon3DDisplay.h \
  Muon3DDisplayLinkDef.h \
  MuonDisplay.h \
  MuonDisplayLinkDef.h \
  MuonEval.h \
  MuonEvalLinkDef.h \
  MuonFvtxEval.h \
  MuonFvtxEvalLinkDef.h

##############################################
# please add new classes in alphabetical order

libmuon_subsysreco_la_SOURCES = \
  Muon3DDisplay.cxx \
  Muon3DDisplayDict.C \
  MuonDisplay.cxx \
  MuonDisplayDict.C \
  MuonEval.cxx \
  MuonEvalDict.C \
  MuonFvtxEval.cxx \
  MuonFvtxEvalDict.C 

noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C
testexternals_LDADD = libmuon_subsysreco.la

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
