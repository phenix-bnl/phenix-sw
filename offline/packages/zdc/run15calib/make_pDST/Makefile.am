AUTOMAKE_OPTIONS = foreign

AM_CXXFLAGS = -Wall -Werror
INCLUDES = -I$(includedir) -I$(OPT_PHENIX)/include -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

lib_LTLIBRARIES = libZDCMakeTree.la

include_HEADERS =
noinst_HEADERS = ZDCMakeTreeLinkDef.h

dist_data_DATA =

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -L$(OPT_PHENIX)/lib

libZDCMakeTree_la_LIBADD = \
  -lCNT \
  -lfun4allfuncs \
  -lSubsysReco \
  -lspin \
  -lert \
  -ltrigger \
  -lnanoDST \
  -llpc \
  -lphool \
  -lTOAD

libZDCMakeTree_la_SOURCES = \
  ZDCMakeTree.C \
  ZDCMakeTree_Dict.C

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libZDCMakeTree.la

testexternals.C:
	@echo '//*** this is a generated file. Do not commit, do not edit' > $@
	@echo 'int main() { }' >> $@

ZDCMakeTree_Dict.C: \
  ZDCMakeTree.h \
  ZDCMakeTreeLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

CLEANFILES = ZDCMakeTree_Dict.C ZDCMakeTree_Dict.h
