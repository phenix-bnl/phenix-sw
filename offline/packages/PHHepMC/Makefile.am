AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I`root-config --incdir` 

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -L$(OPT_PHENIX)/lib

lib_LTLIBRARIES = libPHHepMCNode.la 

include_HEADERS = \
  PHHepMCGenEvent.h \
  PHHepMCHelper.h \
  Fun4AllOscarInputManager.h \
  PHHepMCFilter.h \
  PHHepMCCombiner.h \
  MCHepMCParticle.h \
  MCHepMCParticleContainer.h \
  mFillMCHepMCParticleContainer.h 

libPHHepMCNode_la_LDFLAGS = ${AM_LDFLAGS} `root-config --libs`
libPHHepMCNode_la_LIBADD = \
  -lphool \
  -lSubsysReco \
  -lHepMC \
  -lHepMCfio \
  -lEG \
  -lboost_iostreams \
  -lfun4all

libPHHepMCNode_la_SOURCES = \
  PHHepMCGenEvent.C \
  PHHepMCHelper.C \
  Fun4AllOscarInputManager.C \
  PHHepMCFilter.C \
  PHHepMCCombiner.C \
  MCHepMCParticle.C \
  MCHepMCParticleContainer.C \
  mFillMCHepMCParticleContainer.cc \
  PHHepMCNode_Dict.C

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPHHepMCNode.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHHepMCNode_Dict.C: \
  PHHepMCGenEvent.h \
  PHHepMCHelper.h \
  Fun4AllOscarInputManager.h \
  PHHepMCFilter.h \
  PHHepMCCombiner.h \
  MCHepMCParticle.h \
  MCHepMCParticleContainer.h \
  mFillMCHepMCParticleContainer.h \
  $(OFFLINE_MAIN)/include/HepMC/*.h \
  PHHepMCLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
	rm -f testexternals*
