AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libmutrg.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

noinst_HEADERS = \
  MutrgLinkDef.h

include_HEADERS = \
  MutrgPar.hh \
  MutrgKey.hh \
  MutrgHeader.hh \
  MutrgHeader_v1.hh \
  MutrgHeaderArray.hh \
  MutrgHeaderArray_v1.hh \
  MutrgHit.hh \
  MutrgHit_v1.hh \
  MutrgHit_v2.hh \
  MutrgHit_v3.hh \
  MutrgHitArray.hh \
  MutrgHitArray_v1.hh \
  MutrgTrk.hh \
  MutrgTrk_v1.hh \
  MutrgTrk_v2.hh \
  MutrgTrk_v3.hh \
  MutrgTrkArray.hh \
  MutrgTrkArray_v1.hh \
  MutrgDataProcessUtil.hh \
  MutrgProcessHit.hh \
  MutrgProcessHit_v1.hh \
  MutrgProcessHit_v2.hh \
  MutrgProcessHit_v3.hh \
  MutrgUnpack.hh \
  MutrgDecode.hh \
  MutrgSelectHit.hh \
  MutrgSelectHit_v1.hh \
  MutrgFindTrk.hh \
  MutrgFindTrk_v1.hh \
  MutrgFindTrk_v2.hh \
  MutrgTrkMapping_v1.hh \
  MutrgReco.hh \
  MutrgTrimHit.hh \
  MutrgPackPRDF.hh \
  MutrgEmulateHeader.hh \
  MutrgEmulateHit.hh \
  MutrgSimreco.hh \
  MutrgDBIOTrkMap.hh

libmutrg_la_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib `root-config --glibs`
libmutrg_la_LIBADD = \
  -lphool \
  -lSubsysReco \
  -lheader \
  -lpdbcalBase \
  -lmutoo_base \
  -lmutoo_interface

libmutrg_la_SOURCES = \
  MutrgPar.cc \
  MutrgKey.cc \
  MutrgHeader.cc \
  MutrgHeader_v1.cc \
  MutrgHeaderArray.cc \
  MutrgHeaderArray_v1.cc \
  MutrgHit.cc \
  MutrgHit_v1.cc \
  MutrgHit_v2.cc \
  MutrgHit_v3.cc \
  MutrgHitArray.cc \
  MutrgHitArray_v1.cc \
  MutrgTrk.cc \
  MutrgTrk_v1.cc \
  MutrgTrk_v2.cc \
  MutrgTrk_v3.cc \
  MutrgTrkArray.cc \
  MutrgTrkArray_v1.cc \
  MutrgDataProcessUtil.cc \
  MutrgProcessHit.cc \
  MutrgProcessHit_v1.cc \
  MutrgProcessHit_v2.cc \
  MutrgProcessHit_v3.cc \
  MutrgUnpack.cc \
  MutrgDecode.cc \
  MutrgSelectHit.cc \
  MutrgSelectHit_v1.cc \
  MutrgFindTrk.cc \
  MutrgFindTrk_v1.cc \
  MutrgFindTrk_v2.cc \
  MutrgTrkMapping_v1.cc \
  MutrgReco.cc \
  MutrgTrimHit.cc \
  MutrgPackPRDF.cc \
  MutrgEmulateHeader.cc \
  MutrgEmulateHit.cc \
  MutrgSimreco.cc \
  MutrgDBIOTrkMap.cc \
  MutrgDict.C

dist_data_DATA = \
  mutrg_trigmap/mapping_v2.txt \
  mutrg_trigmap/mapping_v3_sg0.txt \
  mutrg_trigmap/mapping_v3_sg1.txt

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmutrg.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
MutrgDict.C : $(include_HEADERS) MutrgLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	 rm -f $(BUILT_SOURCES) *Dict.{h,C}

