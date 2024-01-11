AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

# no point in suppressing warnings people should
# at least see them, so here we go for g++: -Wall
AM_CXXFLAGS = -Wall -g -msse2

RINCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_CPPFLAGS = -isystem $(includedir) -isystem $(OFFLINE_MAIN)/include -isystem `root-config --incdir`

lib_LTLIBRARIES = libsvxjumpchip.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L`root-config --libdir`

libsvxjumpchip_la_LIBADD = \
  -lnanoDST \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -lSubsysReco \
  -lfun4all \
  -ltrigger \
  -lheader \
  -lbbc \
  -lvtx \
  -lEG

noinst_HEADERS = \
  SvxJumpChipLinkDef.h

include_HEADERS = \
  CorrData.h \
  CorrDataV1.h \
  SvxJumpChipReco.h

libsvxjumpchip_la_SOURCES = \
  CorrData.C \
  CorrDataV1.C \
  SvxJumpChipReco.C \
  SvxJumpChip_Dict.C

# Rule for generating CINT dictionaries from class headers.
SvxJumpChip_Dict.C: \
  CorrData.h \
  CorrDataV1.h \
  SvxJumpChipReco.h \
  SvxJumpChipLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(RINCLUDES) $^

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libsvxjumpchip.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict*
