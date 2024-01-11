AUTOMAKE_OPTIONS = foreign

nodist_SOURCES = testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

lib_LTLIBRARIES = \
  libemcEmbedAfterBurners.la

noinst_HEADERS = \
  EmcRejectAfterBurner.h \
  EmcTofAfterBurnerv1.h \
  PHNodeHelper.h \
  emcEmbedUtils.h \
  mEmcEmbedConst.h \
  nodeEmcEmbed.h \
  PHAfterBurnerLinkDef.h \
  emcEmbedAfterBurnersLinkDef.h

include_HEADERS = \
  emcEmbedTofAfterBurnerv1.h \
  emcEmbedEnergyAfterBurnerv1.h \
  emcEmbedRejectAfterBurnerv1.h \
  emcEmbedRejectAfterBurnerv2.h \
  emcEmbedSimTofAfterBurnerv1.h \
  emcEmbedSimEnergyAfterBurnerv1.h \
  EmcEnergyAfterBurnerv1.h \
  PHAfterBurner.h \
  PHEmcConstants.h

libemcEmbedAfterBurners_la_SOURCES = \
  emcEmbedTofAfterBurnerv1.C \
  emcEmbedEnergyAfterBurnerv1.C \
  emcEmbedRejectAfterBurnerv1.C \
  emcEmbedRejectAfterBurnerv2.C \
  emcEmbedSimTofAfterBurnerv1.C \
  emcEmbedSimEnergyAfterBurnerv1.C \
  emcEmbedUtils.C \
  EmcEnergyAfterBurnerv1.C \
  EmcRejectAfterBurner.C \
  EmcTofAfterBurnerv1.C \
  PHAfterBurner.C \
  PHNodeHelper.C
nodist_libemcEmbedAfterBurners_la_SOURCES = \
  emcEmbedAfterBurners_Dict.C \
  PHAfterBurner_Dict.C

libemcEmbedAfterBurners_la_LDFLAGS = \
  -L$(libdir) -L$(OFFLINE_MAIN)/lib
libemcEmbedAfterBurners_la_LIBADD = \
  -lgea \
  -lheader \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lPISARoot \
  -lWrappers

#noinst_PROGRAMS = \
#  testexternals

testexternals_LDADD = \
  libemcEmbedAfterBurners.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

emcEmbedAfterBurners_Dict.C: \
  emcEmbedTofAfterBurnerv1.h \
  emcEmbedEnergyAfterBurnerv1.h \
  emcEmbedRejectAfterBurnerv1.h \
  emcEmbedRejectAfterBurnerv2.h \
  emcEmbedSimTofAfterBurnerv1.h \
  emcEmbedSimEnergyAfterBurnerv1.h \
  emcEmbedAfterBurnersLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHAfterBurner_Dict.C: \
  EmcEnergyAfterBurnerv1.h \
  EmcRejectAfterBurner.h \
  EmcTofAfterBurnerv1.h  \
  PHAfterBurnerLinkDef.h 
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -rf *Dict*

testexternals_SOURCES = testexternals.C
