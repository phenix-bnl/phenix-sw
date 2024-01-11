AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce
lib_LTLIBRARIES = \
  libphhepmc.la

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib

INCLUDES := \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I`root-config --incdir` \
  -I$(G4_MAIN)/include \
  -I$(G4_MAIN)/include/Geant4

libphhepmc_la_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lfun4all \
  -lHepMC \
  -lflowafterburner \
  -lboost_iostreams \
  -lPHHepMCNode \
  `geant4-config --libs`

include_HEADERS = \
  Fun4AllHepMCInputManager.h \
  Fun4AllHepMCOutputManager.h \
  HepMCFlowAfterBurner.h \
  PHGenEvent.h \
  PHGenEventV1.h \
  PHGenEventList.h \
  PHGenEventListV1.h

libphhepmc_la_SOURCES = \
  Fun4AllHepMCInputManager.C \
  Fun4AllHepMCInputManager_Dict.C \
  Fun4AllHepMCOutputManager.C \
  Fun4AllHepMCOutputManager_Dict.C \
  HepMCFlowAfterBurner.C \
  HepMCFlowAfterBurner_Dict.C \
  PHGenEvent.C \
  PHGenEvent_Dict.C \
  PHGenEventV1.C \
  PHGenEventV1_Dict.C \
  PHGenEventList.C \
  PHGenEventList_Dict.C \
  PHGenEventListV1.C \
  PHGenEventListV1_Dict.C

# Rule for generating table CINT dictionaries.
%_Dict.C: $(OFFLINE_MAIN)/include/HepMC/*.h %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

%_Dict.cpp: $(OFFLINE_MAIN)/include/HepMC/*.h %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

################################################
# linking tests

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = libphhepmc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

##############################################
# please add new classes in alphabetical order

clean-local:
	rm -f *Dict* $(BUILT_SOURCES) testexternals.C
